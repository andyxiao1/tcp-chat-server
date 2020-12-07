module Client where

import Control.Concurrent
import Control.Monad (forever, unless)
import Control.Monad.Fix (fix)
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

-----------------------------
-- Function Declarations
-----------------------------

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
  putStrLn "What is the server host name?"
  host <- getLine
  serverSock <- setupServerSocket host "5000"
  clientLoop serverSock
