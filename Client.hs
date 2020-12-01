module Client (local, client, send) where

import Control.Applicative (Alternative (..), liftA3)
import Control.Monad ()
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe
-- import Data.ProtocolBuffers

-- import qualified Data.Time.Clock as Clock
-- import Network.Info as Info
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
  putStrLn "What port are you publishing over?"
  port <- getLine
  putStrLn "What is your VPN IP address?"
  ip <- getLine
  putStrLn "What is the IP of the server you are connecting to?"
  sip <- getLine
  clientF name port ip sip

-- print connection results
type Username = String

type Port = String

type IPv4 = String

-- loop client

clientF :: Username -> Port -> IPv4 -> IPv4 -> IO Handle
clientF u port h ip = do
  (sa : _) <- getAddrInfo Nothing (Just ip) (Just port)
  sock <- socket (addrFamily sa) Stream defaultProtocol
  connect sock (addrAddress sa)
  handle <- socketToHandle sock WriteMode
  hSetBuffering handle (BlockBuffering Nothing)
  return handle

-- | IP address of the local host
local :: HostName
local = "127.0.0.1"

-- | send a message to the server
send :: Handle -> String -> IO ()
send h c = do
  hPutStrLn h c
  hFlush h