module Server where

import Control.Applicative (Alternative (..), liftA3)
-- import Data.ProtocolBuffers

import Control.Concurrent
import Control.Monad (forever, unless)
-- import ParserCombinators (Parser)
-- import qualified ParserCombinators as P

import Control.Monad.Fix (fix)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack, unpack)
import Data.IORef
import Data.List (delete)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe
import Data.Text (strip)
import qualified Data.Time.Clock as Clock
import GHC.Generics as Gen
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
-- import ParserCombinators (Parser)
-- import qualified ParserCombinators as P
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as PP

-- -----------------------------
-- -- Type Definitions (Model)
-- -----------------------------

-- -- data ThreadMessage = TM Message deriving (Eq, Show)

-- newtype Space = S [Room]

-- data RoomMessage = RM Message Thread

-- data Room = L [RoomMessage] RoomName [Members]

-- data Thread = E | TL [Message] Message

-- data Message = M Sender Content Timestamp

-- -- data SerializedMessage = {}

-- type Content = String

-- type Timestamp = Clock.UTCTime

-- type Sender = String

-- type RoomName = String

-- type Port = Int

-- newtype Members = Mem [Sender]

-- type IPv4 = String

-- type Store = Map RoomName Room

-- -- is it better to store users in rooms to avoid map lookup?
-- -- newtype Users = Map Sender (IPv4, Port)

-----------------------------
-- Type Definitions (Model)
-----------------------------

type RoomName = String

-- data User = U {username :: Username, conn :: Socket} deriving (Eq)

-- instance Show User where
--   show u = show (username u) ++ show (conn u)
type User = String

type Username = String

type MessageContent = String

type RoomStore = Map RoomName Room

type UserStore = Map User (Chan Message)

type ServerState = MVar (RoomStore, UserStore)

data Room = R {name :: String, messages :: [Message], users :: [User], channel :: Chan Message}

data Message = M {sender :: User, content :: String} deriving (Eq)

instance Show Message where
  show msg = sender msg ++ ": " ++ content msg

emptyStore :: (RoomStore, UserStore)
emptyStore = (Map.empty, Map.empty)

-----------------------------
-- Function Declarations
-----------------------------

getAllRooms :: ServerState -> IO [RoomName]
getAllRooms state = do
  (roomStore, _) <- readMVar state
  return $ Map.keys roomStore

getAllRoomMessages :: ServerState -> RoomName -> IO [Message]
getAllRoomMessages state room = do
  (roomStore, _) <- readMVar state
  return $ maybe [] messages (Map.lookup room roomStore)

getUserChannel :: ServerState -> User -> IO (Chan Message)
getUserChannel state user = do
  (_, userStore) <- readMVar state
  case Map.lookup user userStore of
    Nothing -> error $ "error: " ++ user ++ " has no channel."
    Just chan -> return chan

createRoom :: ServerState -> RoomName -> IO ()
createRoom state roomName = do
  (roomStore, userStore) <- takeMVar state
  -- TODO: check if room already exists
  chan <- newChan
  let newRoom = R roomName [] [] chan
  putMVar state (Map.insert roomName newRoom roomStore, userStore)

-- | Add user to room user list and set the user's channel to a duplicate of the rooms channel.
-- TODO: check if room doesn't exists or if user is already in room (maybe don't need).
addUserToRoom :: ServerState -> User -> RoomName -> IO ()
addUserToRoom state usr roomName = do
  store@(roomStore, userStore) <- takeMVar state
  case Map.lookup roomName roomStore of
    Nothing -> putMVar state store
    Just room -> do
      commLine <- dupChan $ channel room
      let newRoomStore = Map.insert roomName (updateRoom room) roomStore
          newUserStore = Map.insert usr commLine userStore
      putMVar state (newRoomStore, newUserStore)
  where
    updateRoom rm@(R name messages users channel) =
      if usr `elem` users
        then rm
        else R name messages (usr : users) channel

-- | Add message to room message list and room channel.
-- TODO this function should also send messages to the connections associated with users
sendRoomMessage :: ServerState -> User -> RoomName -> MessageContent -> IO ()
sendRoomMessage state usr roomName msg = do
  store@(roomStore, userStore) <- takeMVar state
  case Map.lookup roomName roomStore of
    Nothing -> putMVar state store
    Just (R name messages users channel) -> do
      let message = M usr msg
          newRoom = R name (messages ++ [message]) users channel
          newRoomStore = Map.insert roomName newRoom roomStore
      writeChan channel message
      putMVar state (newRoomStore, userStore)

-- TODO: implement delete room
-- deleteRoom :: RoomName -> State Store ()
-- deleteRoom r = do
--   store <- S.get
--   S.put (Map.delete r store)

-- TODO: implement removeUserFromRoom.
-- removeUserFromRoom :: User -> RoomName -> State Store ()
-- removeUserFromRoom usr room = do
--   store <- S.get
--   S.put (Map.adjust (\(R name messages users _) -> R name messages (delete usr users)) room store)

-- TODO: implement switchUserBetweenRooms.
-- switchUserBetweenRooms :: User -> RoomName -> RoomName -> State Store ()
-- switchUserBetweenRooms usr r1 r2 = do
--   removeUserFromRoom usr r1
--   addUserToRoom usr r2

-----------------------------
-- Test Cases
-----------------------------

-- tMsgConversion :: Test
-- tMsgConversion = undefined

-- prop_verifySend :: Sender -> String -> Room -> Bool
-- prop_verifySend s str rm@(RM (x@(M _ c _) : _) rn mem) = do
--   sendMsgRm s str rm
--   -- get `Message` from `x` which is a `RoomMessage`
--   -- get `Content` from that `Message`
--   c == str

-- case head rms of
--   RM msg th -> msg == str

-- clientThread :: Socket -> IO ()
-- clientThread sock = do
--   -- -- get user's name
--   -- sendAll sock "What is your name?"
--   -- name <- recv sock 1024
--   -- -- get room to add user to
--   -- sendAll sock (show getAllRooms)
--   -- sendAll sock "What room would you like to join?"
--   -- rm <- strip (recv sock 1024)
--   -- -- create user
--   -- addUserToRoom name rm
--   msg <- recv sock 1024
--   unless (B.null msg) $ do
--     sendAll sock msg
--     clientThread sock

-- network :: IPv4 -> IO ()
-- network ip = do
--   withSocketsDo $ do
--     Prelude.putStrLn "Opening a socket."
--     addr <- resolve
--     sock <- open addr
--     (conn, _peer) <- accept sock
--     Prelude.putStrLn "Connected to socket."
--     -- get user's name
--     sendAll sock (pack "What is your name?")
--     name <- recv sock 1024
--     -- get room to add user to
--     sendAll sock (pack (show getAllRooms))
--     sendAll sock (pack "What room would you like to join?")
--     rm <- recv sock 1024
--     -- create user
--     s <- addUserToRoom name (unpack rm)
--     --spawn a new process here
--     -- TODO: how to share state store between processes?
--     forkFinally (clientThread conn) (const $ gracefulClose conn 5000)
--     network ip
--   where
--     -- -- interface
--     -- --   mv
--     -- --   ( atom $ do
--     -- --       x <- hReady handle
--     -- --       if x
--     -- --         then Just <$> hGetLine handle
--     -- --         else return Nothing
--     -- --   )
--     -- atom $ do
--     --   hClose handle
--     --   putStrLn "Socket closed."

--     resolve = do
--       let hints =
--             defaultHints
--               { addrFlags = [AI_PASSIVE],
--                 addrSocketType = Stream
--               }
--       addrInfos <- getAddrInfo (Just hints) (Just ip) (Just "5000")
--       case addrInfos of
--         [] -> error "resolve returned no results"
--         (addrInfo : _) -> return addrInfo
--     open addr = do
--       sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
--       setSocketOption sock ReuseAddr 1
--       withFdSocket sock setCloseOnExecIfNeeded
--       bind sock $ addrAddress addr
--       listen sock 1024
--       return sock

-- main :: IO ()
-- main = do
--   Prelude.putStrLn "What is your VPN IP address?"
--   ip <- Prelude.getLine
--   -- create socket
--   (sa : _) <- getAddrInfo Nothing (Just ip) (Just "5000")
--   sock <- socket (addrFamily sa) Stream defaultProtocol
--   connect sock (addrAddress sa)
--   listen sock 1024

-- loop

data Action
  = GetAllRooms
  | GetAllRoomMessages RoomName
  | CreateRoom RoomName
  | -- | DeleteRoom RoomName
    AddUserToRoom Username RoomName
  | SwitchUserBetweenRooms Username RoomName RoomName
  | SendRoomMessage Username RoomName MessageContent
  deriving (Eq, Show)

-- actionParser :: Parser Action

-- | Takes in ip address and creates + sets up the socket that listens for new connections.
setupConnSocket :: HostName -> IO Socket
setupConnSocket ip = do
  putStrLn "Opening connection socket..."
  addr <- getAddr
  connSock <- openSocket addr
  putStrLn "Connection socket has been setup."
  return connSock
  where
    getAddr = do
      let hints =
            defaultHints
              { addrFlags = [AI_PASSIVE],
                addrSocketType = Stream
              }
      addrInfos <- getAddrInfo (Just hints) (Just ip) (Just "5000")
      case addrInfos of
        [] -> error "Error getting address"
        (addrInfo : _) -> return addrInfo
    openSocket addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      return sock

-- | Loop that waits for new connection requests and spawns a new thread for each new connection.
connLoop :: Socket -> ServerState -> IO ()
connLoop connSock state = forever $ do
  (clientSock, clientAddr) <- accept connSock
  forkFinally (clientLoop clientSock clientAddr state) (const $ gracefulClose clientSock 5000)

-- | Loop that handles a single client.
clientLoop :: Socket -> SockAddr -> ServerState -> IO ()
clientLoop clientSock clientAddr state = do
  putStrLn $ "Connected to client: " ++ show clientAddr
  let sendMsg msg = sendAll clientSock $ pack msg
      recvMsg = do
        byteStr <- recv clientSock 1024
        return $ unpack byteStr

  -- Get user name.
  sendMsg "What is your name?"
  user <- recvMsg

  createRoom state "base"

  -- Ask user to join room initially.
  sendMsg "What room would you like to join?"
  room <- recvMsg

  addUserToRoom state user room

  -- Thread to listen for new messages from the user's channel.
  reader <- forkIO $
    forever $ do
      commLine <- getUserChannel state user
      -- TODO: [BUG] I think this will block so if a user switches rooms this thread will be in a weird state. So maybe this won't work??
      nextMessage <- readChan commLine
      sendMsg $ show nextMessage

  -- Thread to listen for messages from the client.
  fix $ \loop -> do
    msg <- recvMsg
    case msg of
      "q" -> sendMsg "Bye!"
      _ -> do
        sendRoomMessage state user room msg
        loop

  killThread reader

-- let broadcast msg = writeChan chan msg
-- broadcast ("--> " ++ name ++ " entered chat.")
-- broadcast ("<-- " ++ name ++ " left.")

main :: IO ()
main = do
  putStrLn "What is your VPN IP address?"
  ip <- getLine
  connSocket <- setupConnSocket ip
  state <- newMVar emptyStore
  connLoop connSocket state