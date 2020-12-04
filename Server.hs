module Server where

import Control.Applicative (Alternative (..), liftA3)
import Control.Monad ()
-- import Data.ProtocolBuffers

import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe
import qualified Data.Time.Clock as Clock
import GHC.Generics as Gen
import Network.Socket
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as PP

-----------------------------
-- Type Definitions (Model)
-----------------------------

-- data ThreadMessage = TM Message deriving (Eq, Show)

newtype Space = S [Room]

data RoomMessage = RM Message Thread

data Room = L [RoomMessage] RoomName [Members]

data Thread = E | TL [Message] Message

data Message = M Sender Content Timestamp

-- data SerializedMessage = {}

type Content = String

type Timestamp = Clock.UTCTime

type Sender = String

type RoomName = String

type Port = Int

newtype Members = Mem [Sender]

type IPv4 = String

-- is it better to store users in rooms to avoid map lookup?
-- newtype Users = Map Sender (IPv4, Port)

-----------------------------
-- Function Declarations
-----------------------------

-- minimize io usage

type MVar a = IORef (Maybe a)

data Action
  = Atom (IO Action) -- an atomic computation, returning a new action
  | Fork Action Action -- create a new thread
  | Stop -- terminate this thread

writeAction :: String -> Action
writeAction = Action

prog :: Action
prog = Fork (writeAction "Hello\n") (writeAction "CIS 552\n")

newtype C a = C {runC :: (a -> Action) -> Action}

atom :: IO a -> Server.C a
atom x = C $ \k -> k x

class Monad m => MVarMonad m where
  newMVar :: m (MVar a)
  writeMVar :: MVar a -> a -> m ()
  takeMVar :: MVar a -> m (Maybe a)

instance MVarMonad IO where
  newMVar = newIORef Nothing
  writeMVar v a = writeIORef v (Just a)
  takeMVar a = readIORef a

-- instance MVarMonad C where
--   newMVar = atom newMVar
--   writeMVar v a = atom (writeMVar v a)
--   takeMVar v = atom (takeMVar v)

-- listen :: IO ()
-- listen = undefined

recvMsg :: IPv4 -> Port -> String -> IO ()
recvMsg = undefined

connectSenderToRoom :: IPv4 -> Port -> RoomName -> IO ()
connectSenderToRoom = undefined

removeSenderFromRoom :: Sender -> RoomName -> IO ()
removeSenderFromRoom = undefined

switchSenderBWRooms :: Sender -> RoomName -> RoomName -> IO ()
switchSenderBWRooms s r1 r2 = do
  removeSenderFromRoom s r1
  connectSenderToRoom s r2

getRooms :: Space
getRooms = undefined

sendMsgRm :: Sender -> String -> Room -> IO ()
sendMsgRm = undefined

sendMsgThread :: Sender -> String -> Thread -> IO ()
sendMsgThread = undefined

sendMsg :: Sender -> String -> IO ()
sendMsg = undefined

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

main :: IO ()
main = do
  putStrLn "What port are you publishing over?"
  port <- getLine
  putStrLn "What is your VPN IP address?"
  ip <- getLine
  -- create socket
  (sa : _) <- getAddrInfo Nothing (Just ip) (Just port)
  sock <- socket (addrFamily sa) Stream defaultProtocol
  connect sock (addrAddress sa)
  -- loop
  loop sock
  where
    loop sock =
      listen
        sock
        1024
        -- check for new connections + add to current list of members
        -- move users around if need be
        -- send messages between users
        loop
        sock
