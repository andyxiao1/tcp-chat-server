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
import Network.Socket
import System.IO
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as PP

-- maintain messages & other simple shared data types

type MVar a = IORef (Maybe a)

data Action
  = Atom (IO Action) -- an atomic computation, returning a new action
  | Fork Action Action -- create a new thread
  | Stop -- terminate this thread

newtype C a = C {runC :: (a -> Action) -> Action}

atom :: IO a -> C a
atom a = C $ runC Atom a

class Monad m => MVarMonad m where
  newMVar :: m (MVar a)
  writeMVar :: MVar a -> a -> m ()
  takeMVar :: MVar a -> m (Maybe a)

instance MVarMonad IO where
  newMVar = newIORef Nothing
  writeMVar v a = writeIORef v (Just a)
  takeMVar a = readIORef a

instance MVarMonad C where
  newMVar = atom newMVar
  writeMVar v a = atom (writeMVar v a)
  takeMVar v = atom (takeMVar v)

-- Functions

main :: IO ()
main = do
  -- run with command line arguments of: name, port
  putStrLn "what is your name?"
  name <- getLine
  putStrLn "What port are you publishing over?"
  port <- getLine
  putStrLn "What is your VPN IP address?"
  ip <- getLine
  return

-- network :: String -> MVar Msg -> IO ()
-- network port mv = do
--     handle <- atom $ withSocketsDo $ do
--                     putStrLn "Opening a socket."
--                     addr <- resolve
--                     sock <- open addr
--                     (conn,_peer) <- accept sock
--                     putStrLn "Connected to socket."
--                     socketToHandle conn ReadMode
--     interface mv (atom $ do
--                             x <- hReady handle
--                             if x then Just <$> hGetLine handle
--                             else return Nothing)
--     atom $ do hClose handle
--             putStrLn "Socket closed."
-- where
--     resolve = do
--         let hints = defaultHints {
--                 addrFlags = [AI_PASSIVE]
--             , addrSocketType = Stream
--             }
--         addrInfos <- getAddrInfo (Just hints) Nothing (Just port)
--         case addrInfos of
--         [] -> error "resolve returned no results"
--         (addrInfo:_) -> return addrInfo
--     open addr = do
--         sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
--         setSocketOption sock ReuseAddr 1
--         withFdSocket sock setCloseOnExecIfNeeded
--         bind sock $ addrAddress addr
--         listen sock 1024
--         return sock