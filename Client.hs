module Client where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Monad (forever, unless, void)
import Control.Monad.Fix (fix)
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Char8 as C8
import qualified Graphics.Vty as V
import Lens.Micro.Platform (makeLenses, (^.))
import Network.Socket
  ( AddrInfo (addrAddress, addrFamily, addrProtocol, addrSocketType),
    HostName,
    ServiceName,
    Socket,
    SocketType (Stream),
    connect,
    defaultHints,
    getAddrInfo,
    socket,
  )
import Network.Socket.ByteString (recv, sendAll)
import TUI (BChan, ServerResponse (..), app, customMain, initState, newBChan, writeBChan)

-----------------------------
-- Network Functions
-----------------------------

-- | Creates socket connection to server.
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

-- | Spawns thread to listen to network messages.
startNetworkListener :: Socket -> BChan ServerResponse -> IO ThreadId
startNetworkListener serverSock chan =
  forkIO $
    forever $ do
      nextLine <- recv serverSock 1024
      writeBChan chan $ SR (unpack nextLine)

-----------------------------
-- Main
-----------------------------

-- | Runs client application.
main :: IO ()
main = do
  -- Get initial user input.
  putStrLn "What is your name?"
  name <- getLine
  putStrLn "What is the server host name?"
  host <- getLine

  -- Setup Socket + TUI.
  serverSock <- setupServerSocket host "5000"
  chan <- newBChan 10
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty

  -- Start network listener thread.
  listenerID <- startNetworkListener serverSock chan

  -- Send initial data to server.
  sendAll serverSock $ C8.pack name

  -- Start TUI thread.
  customMain initialVty buildVty (Just chan) app $ initState name serverSock

  -- Cleanup.
  killThread listenerID