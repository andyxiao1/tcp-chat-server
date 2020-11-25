module Server where

import Control.Applicative (Alternative (..), liftA3)
import Control.Monad ()
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe
-- import Data.ProtocolBuffers

import qualified Data.Time.Clock as Clock
import GHC.Generics as Gen
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as PP

-----------------------------
-- Type Definitions (Model)
-----------------------------

-- data ThreadMessage = TM Message deriving (Eq, Show)

newtype Space = S [Room]

data RoomMessage = RM Message Thread deriving (Eq, Show)

data Room = L [RoomMessage] RoomName [Members] deriving (Eq, Show)

data Thread = E | TL [Message] Message deriving (Eq, Show)

data Message = M Sender Content Timestamp

-- data SerializedMessage = {}

type Content = String

type Timestamp = Clock.UTCTime

type Sender = String

type RoomName = String

type Port = Int

newtype Members = Mem [Sender]

newtype IPv4 = String

-- is it better to store users in rooms to avoid map lookup?
newtype Users = Map Sender (IPv4, Port)

-----------------------------
-- Function Declarations
-----------------------------

-- minimize io usage

listen :: IO ()
listen = undefined

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

tMsgConversion :: Test
tMsgConversion = undefined

prop_verifySend :: Sender -> String -> Room -> Bool
prop_verifySend s str rm@(RM (x@(M _ c _) : _) rn mem) = do
  sendMsgRm s str rm
  -- get `Message` from `x` which is a `RoomMessage`
  -- get `Content` from that `Message`
  c == str

-- case head rms of
--   RM msg th -> msg == str