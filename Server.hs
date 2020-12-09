module Server where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever, unless)
import Control.Monad.Fix (fix)
import qualified Data.ByteString.Char8 as C
import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

-- import ParserCombinators (Parser)
-- import qualified ParserCombinators as P
-- import Text.PrettyPrint (Doc)
-- import qualified Text.PrettyPrint as PP
-- import qualified Data.Time.Clock as Clock

-----------------------------
-- Type Definitions (Model)
-----------------------------

type RoomName = String

type User = String

type MessageContent = String

type Response = String

type RoomStore = Map RoomName Room

type UserStore = Map User (TChan Message, RoomName)

type ServerState = TVar (RoomStore, UserStore)

data Room = R {name :: String, messages :: [Message], users :: [User], channel :: TChan Message}

data Message = M {sender :: User, content :: String} deriving (Eq)

instance Show Message where
  show msg = sender msg ++ ": " ++ content msg

emptyStore :: (RoomStore, UserStore)
emptyStore = (Map.empty, Map.empty)

-----------------------------
-- Function Declarations
-----------------------------

-- TODO: make the other functions pure functions and use something like this to wrap it into the STM Monad
-- TODO: this will make it so we can test the other functions we might need to return a roomstore/userstore and write? or have multiple different wrapper functions
stmWrapper :: ServerState -> ((RoomStore, UserStore) -> [String]) -> STM [String]
stmWrapper state pureFn = do
  store <- readTVar state
  return $ pureFn store

getAllRooms :: ServerState -> STM [RoomName]
getAllRooms state = do
  (roomStore, _) <- readTVar state
  return $ Map.keys roomStore

getAllRoomMessages :: ServerState -> RoomName -> STM [String]
getAllRoomMessages state room = do
  (roomStore, _) <- readTVar state
  return $ maybe [] (fmap show . messages) (Map.lookup room roomStore)

getUserChannel :: ServerState -> User -> STM (TChan Message)
getUserChannel state user = do
  (_, userStore) <- readTVar state
  case Map.lookup user userStore of
    Nothing -> error $ "error: " ++ user ++ " has no channel."
    Just (tchan, _) -> return tchan

getUserRoom :: ServerState -> User -> STM RoomName
getUserRoom state user = do
  (_, userStore) <- readTVar state
  case Map.lookup user userStore of
    Nothing -> error $ "error: " ++ user ++ " has no channel."
    Just (_, room) -> return room

createRoom :: ServerState -> RoomName -> STM [Response]
createRoom state roomName = do
  (roomStore, userStore) <- readTVar state
  -- TODO: check if room already exists + chan is a memory leak because nothing reads it.
  tchan <- newTChan
  let newRoom = R roomName [] [] tchan
  writeTVar state (Map.insert roomName newRoom roomStore, userStore)
  return []

-- | Add user to room user list and set the user's channel to a duplicate of the rooms channel.
-- TODO: check if room doesn't exists or if user is already in room (maybe don't need).
addUserToRoom :: ServerState -> User -> RoomName -> STM [Response]
addUserToRoom state usr roomName = do
  (roomStore, userStore) <- readTVar state
  case Map.lookup roomName roomStore of
    Nothing -> return []
    Just room -> do
      commLine <- dupTChan $ channel room
      let newRoomStore = Map.insert roomName (updateRoom room) roomStore
          newUserStore = Map.insert usr (commLine, roomName) userStore
      writeTVar state (newRoomStore, newUserStore)
      getAllRoomMessages state roomName
  where
    updateRoom rm@(R name messages users channel) =
      if usr `elem` users
        then rm
        else R name messages (usr : users) channel

-- | Add message to room message list and room channel.
-- TODO this function should also send messages to the connections associated with users
sendRoomMessage :: ServerState -> User -> RoomName -> MessageContent -> STM [Response]
sendRoomMessage state usr roomName msg = do
  (roomStore, userStore) <- readTVar state
  case Map.lookup roomName roomStore of
    Nothing -> return []
    Just (R name messages users channel) -> do
      let message = M usr msg
          newRoom = R name (messages ++ [message]) users channel
          newRoomStore = Map.insert roomName newRoom roomStore
      writeTChan channel message
      writeTVar state (newRoomStore, userStore)
      return []

-- | Handles client input.
handleInput :: ServerState -> User -> MessageContent -> STM [Response]
handleInput state usr msg = do
  roomName <- getUserRoom state usr
  case T.unpack (T.strip (T.pack msg)) of
    ':' : command -> case command of
      -- create room
      'n' : ' ' : rm -> createRoom state rm
      -- switch room
      -- 's' : ' ' : rm -> switchUserBetweenRooms state usr roomName rm
      's' : ' ' : rm -> addUserToRoom state usr rm -- TODO: temporary replace after implementing `switchUserBetweenRooms`
      -- see all rooms
      "g" -> getAllRooms state
      -- unknown command
      _ -> return []
    -- send message to room
    message -> sendRoomMessage state usr roomName message

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
  let sendMsg msg = sendAll clientSock $ C.pack msg
      recvMsg = do
        byteStr <- recv clientSock 1024
        return $ C.unpack byteStr
      sendResponses _ [] = return ()
      sendResponses user (resp : xs) = do
        if user `isPrefixOf` resp
          then sendResponses user xs
          else do
            sendMsg $ resp ++ "\n"
            sendResponses user xs

  -- Get user name.
  sendMsg "What is your name?"
  user <- recvMsg

  -- Ask user to join room initially.
  sendMsg "What room would you like to join?"
  room <- recvMsg

  -- clear the client's screen
  sendMsg "clear"

  responses <- atomically $ addUserToRoom state user room
  sendResponses user responses

  -- Thread to listen for new messages from the user's channel.
  reader <- forkIO $
    forever $ do
      commLine <- atomically $ getUserChannel state user
      isEmpty <- atomically $ isEmptyTChan commLine
      -- Only read from channel if not empty, then send message to client.
      unless isEmpty $ do
        nextMessage <- atomically $ readTChan commLine
        sendMsg $ show nextMessage

  -- Thread to listen for messages from the client.
  fix $ \loop -> do
    msg <- recvMsg
    case msg of
      ":q" -> sendMsg "Bye!"
      ':' : 's' : _ -> do
        sendMsg "clear"
        responses <- atomically $ handleInput state user msg
        sendResponses user responses
        loop
      _ -> do
        responses <- atomically $ handleInput state user msg
        sendResponses user responses
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
  state <- newTVarIO emptyStore
  atomically $ createRoom state "base"
  connLoop connSocket state

-- TODO: Create `Action` parser. Maybe we don't need anymore?
-- data Action
--   = GetAllRooms
--   | GetAllRoomMessages RoomName
--   | CreateRoom RoomName
--   | -- | DeleteRoom RoomName
--     AddUserToRoom User RoomName
--   | SwitchUserBetweenRooms User RoomName RoomName
--   | SendRoomMessage User RoomName MessageContent
--   deriving (Eq, Show)
-- actionParser :: Parser Action

-----------------------------
-- Test Cases
-----------------------------
-- TODO: Add test cases.