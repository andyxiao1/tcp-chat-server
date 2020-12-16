module Client where

import Control.Applicative (Alternative (..), liftA3)
-- import Data.ProtocolBuffers

-- import qualified Data.Time.Clock as Clock
-- import Network.Info as Info

import Control.Concurrent
import Control.Monad (forever, unless)
import Control.Monad.Fix (fix)
import qualified Data.ByteString.Char8 as C
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe
import Data.Text (pack, strip, unpack)
import Network.Socket
import Network.Socket hiding (send)
import Network.Socket.ByteString (recv, sendAll)
import System.IO
  ( BufferMode (BlockBuffering),
    Handle,
    IOMode (WriteMode),
    hFlush,
    hPutStrLn,
    hSetBuffering,
  )
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as PP

-- maintain messages & other simple shared data types

-- Functions

client :: IO Handle
client = do
  -- run with command line arguments of: name, port
  putStrLn "what is your name?"
  name <- getLine
  putStrLn "What port do you want to connect to?"
  port <- getLine
  putStrLn "What is the IP of the server you are connecting to?"
  ip <- getLine
  clientF name port ip

-- print connection results
type Username = String

type Port = String

type IPv4 = String

type RoomName = String

type User = String

type MessageContent = String

data Action
  = GetAllRooms
  | GetAllRoomMessages RoomName
  | CreateRoom RoomName
  | -- | DeleteRoom RoomName
    AddUserToRoom Username RoomName
  | SwitchUserBetweenRooms Username RoomName RoomName
  | SendRoomMessage Username RoomName MessageContent
  | Quit
  deriving (Eq, Show)

-- loop client

clientF :: Username -> Port -> IPv4 -> IO Handle
clientF u port ip = do
  (sa : _) <- getAddrInfo Nothing (Just ip) (Just port)
  sock <- socket (addrFamily sa) Stream defaultProtocol
  connect sock (addrAddress sa)
  handle <- socketToHandle sock WriteMode
  hSetBuffering handle (BlockBuffering Nothing)
  -- print out all rooms
  -- ask user to join room
  let rm = "room1"
  -- check if the room they tried to join exists
  run u rm handle
  where
    run u rm handle = do
      putStr "Client> "
      msg <- getLine -- don't want this to be a blocking statement
      let action = createAction u rm msg
      -- if action is `Quit` end the loop
      send handle (show action)
      -- if current action is `SwitchUserBetweenRooms` then check if nw request worked and update room name in recursive call
      run u rm handle

-- | send a message to the server
send :: Handle -> String -> IO ()
send h c = do
  hPutStrLn h c
  hFlush h

createAction :: User -> RoomName -> MessageContent -> Action
createAction u r m =
  case unpack (strip (pack m)) of
    ':' : command -> case command of
      -- create room
      'n' : ' ' : rm -> CreateRoom rm
      -- delete room
      -- 'd' : ' ' : rm -> DeleteRoom rm
      -- join room
      'u' : ' ' : rm -> AddUserToRoom u rm
      -- switch room
      's' : ' ' : rm -> SwitchUserBetweenRooms u r rm
      -- see all rooms
      "g" -> GetAllRooms
      -- quit
      "q" -> Quit
      -- unknown command
      _ -> undefined -- putStrLn "?" >> go store
      -- send message to room
    msg -> SendRoomMessage u r msg

setupServerSocket :: HostName -> ServiceName -> IO Socket
setupServerSocket host port = do
  putStrLn "Opening connection to server..."
  addr <- getAddr
  serverSock <- openSocket addr
  putStrLn "Connected to server."
  return serverSock
  where
    getAddr = do
      let hints = defaultHints {addrSocketType = Stream}
      addrInfos <- getAddrInfo (Just hints) (Just host) (Just port)
      case addrInfos of
        [] -> error "Error getting address"
        (addrInfo : _) -> return addrInfo
    openSocket addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock $ addrAddress addr
      return sock

clientLoop :: Socket -> IO ()
clientLoop serverSock = do
  -- Thread for listening to messages from the server.
  reader <- forkIO $
    forever $ do
      nextLine <- recv serverSock 1024
      C.putStrLn nextLine

  -- Thread to listen for user input to send to the server.
  fix $ \loop -> do
    msg <- getLine
    sendAll serverSock $ C.pack msg
    unless (msg == ":q") loop

  killThread reader

main :: IO ()
main = do
  putStrLn "What is server host name?"
  host <- getLine
  serverSock <- setupServerSocket host "5000"
  clientLoop serverSock

clientLoop :: Socket -> IO ()
clientLoop serverSock = do
  -- Thread for listening to messages from the server.
  reader <- forkIO $
    forever $ do
      nextLine <- recv serverSock 1024
      if "clear" `isPrefixOf` C8.unpack nextLine
        then do
          clearScreen
          C8.putStrLn (C8.pack (drop 5 (C8.unpack nextLine)))
        else C8.putStrLn nextLine

  -- void $ defaultMain app exampleState
  -- Thread to listen for user input to send to the server.
  fix $ \loop -> do
    msg <- getLine
    cursorUpLine 1
    clearLine
    sendAll serverSock $ C8.pack msg
    unless (msg == ":q") loop

-- data Action
--   = GetAllRooms
--   | GetAllRoomMessages RoomName
--   | CreateRoom RoomName
--   | -- | DeleteRoom RoomName
--     AddUserToRoom Username RoomName
--   | SwitchUserBetweenRooms Username RoomName RoomName
--   | SendRoomMessage Username RoomName MessageContent
--   | Quit
--   deriving (Eq, Show)