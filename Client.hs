module Client where

import Control.Concurrent
import Control.Monad (forever, unless)
import Control.Monad.Fix (fix)
import qualified Data.ByteString.Char8 as C
import Data.List (isPrefixOf)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Console.ANSI
import System.IO

-----------------------------
-- Function Declarations
-----------------------------

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
      if "clear" `isPrefixOf` C.unpack nextLine
        then do
          clearScreen
          C.putStrLn (C.pack (drop 5 (C.unpack nextLine)))
        else C.putStrLn nextLine

  -- Thread to listen for user input to send to the server.
  fix $ \loop -> do
    msg <- getLine
    cursorUpLine 1
    clearLine
    sendAll serverSock $ C.pack msg
    unless (msg == ":q") loop

  killThread reader

main :: IO ()
main = do
  putStrLn "What is the server host name?"
  host <- getLine
  serverSock <- setupServerSocket host "5000"
  clientLoop serverSock
