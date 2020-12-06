module Server where

import Control.Applicative (Alternative (..), liftA3)
-- import Data.ProtocolBuffers

import Control.Concurrent
import Control.Monad ()
import Data.IORef
import Data.List (delete)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe
import qualified Data.Time.Clock as Clock
import GHC.Generics as Gen
import Network.Socket
import ParserCombinators (Parser)
import qualified ParserCombinators as P
import State (State)
import qualified State as S
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

type IPv4 = String

-- type Store = Map RoomName Room

-- -- is it better to store users in rooms to avoid map lookup?
-- -- newtype Users = Map Sender (IPv4, Port)

-----------------------------
-- Type Definitions (Model)
-----------------------------

type RoomName = String

type User = String

type Username = String

type MessageContent = String

type Store = Map RoomName Room

data Room = R {name :: String, messages :: [Message], users :: [User]}

data Message = M {sender :: User, content :: String} deriving (Show)

emptyStore :: Store
emptyStore = Map.empty

-----------------------------
-- Function Declarations
-----------------------------

getAllRooms :: Store -> [RoomName]
getAllRooms = Map.keys

getAllRoomMessages :: Store -> RoomName -> [Message]
getAllRoomMessages store room = case Map.lookup room store of
  Nothing -> []
  Just (R _ messages _) -> messages

createRoom :: RoomName -> State Store ()
createRoom r = do
  store <- S.get
  S.put (Map.insert r (R r [] []) store)

deleteRoom :: RoomName -> State Store ()
deleteRoom r = do
  store <- S.get
  S.put (Map.delete r store)

addUserToRoom :: User -> RoomName -> State Store ()
addUserToRoom usr room = do
  store <- S.get
  S.put (Map.adjust updateRoom room store)
  where
    updateRoom rm@(R name messages users) =
      if usr `elem` users
        then rm
        else R name messages (usr : users)

removeUserFromRoom :: User -> RoomName -> State Store ()
removeUserFromRoom usr room = do
  store <- S.get
  S.put (Map.adjust (\(R name messages users) -> R name messages (delete usr users)) room store)

switchUserBetweenRooms :: User -> RoomName -> RoomName -> State Store ()
switchUserBetweenRooms usr r1 r2 = do
  removeUserFromRoom usr r1
  addUserToRoom usr r2

sendRoomMessage :: User -> RoomName -> MessageContent -> State Store ()
sendRoomMessage usr room msg = do
  store <- S.get
  S.put (Map.adjust updateRoom room store)
  where
    updateRoom rm@(R name messages users) =
      if usr `notElem` users
        then rm
        else R name (M usr msg : messages) users

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

clientThread :: Socket -> IO a
clientThread sock = undefined

network :: IPv4 -> IO ()
network ip = do
  withSocketsDo $ do
    putStrLn "Opening a socket."
    addr <- resolve
    sock <- open addr
    (conn, _peer) <- accept sock
    putStrLn "Connected to socket."
    --spawn a new process here
    forkFinally (clientThread conn) (const $ gracefulClose conn 5000)
    network ip
  where
    -- -- interface
    -- --   mv
    -- --   ( atom $ do
    -- --       x <- hReady handle
    -- --       if x
    -- --         then Just <$> hGetLine handle
    -- --         else return Nothing
    -- --   )
    -- atom $ do
    --   hClose handle
    --   putStrLn "Socket closed."

    resolve = do
      let hints =
            defaultHints
              { addrFlags = [AI_PASSIVE],
                addrSocketType = Stream
              }
      addrInfos <- getAddrInfo (Just hints) (Just ip) (Just "5000")
      case addrInfos of
        [] -> error "resolve returned no results"
        (addrInfo : _) -> return addrInfo
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      return sock

main :: IO ()
main = do
  putStrLn "What is your VPN IP address?"
  ip <- getLine
  -- create socket
  (sa : _) <- getAddrInfo Nothing (Just ip) (Just "5000")
  sock <- socket (addrFamily sa) Stream defaultProtocol
  connect sock (addrAddress sa)
  listen sock 1024

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