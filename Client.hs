module Client where

import Control.Applicative (Alternative (..), liftA3)
import Control.Monad ()
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe
-- import Data.ProtocolBuffers

-- import qualified Data.Time.Clock as Clock
-- import Network.Info as Info

import Data.Text (pack, strip, unpack)
import Network.Socket hiding (send)
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