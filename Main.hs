module Main where

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

main :: IO ()
main = return ()

-------------------------------------------------------------------------
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

newtype Members = Mem [Sender]

-----------------------------
-- Function Declarations
-----------------------------

-----------------------------
-- Test Cases
-----------------------------
