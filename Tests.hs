-- Testing code for WHILE language
-- Add your own test cases to this file
-- Make sure that you read the .lhs version for clarification
module Tests where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever, unless)
import Control.Monad.Fix (fix)
import qualified Data.ByteString.Char8 as C
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Server
import Test.HUnit

-----------------------------------------------------------------
-- A main action to run all the tests...

main :: IO ()
main = do
  return ()

------------------------- Test cases for the interpreter -----
-----------------------------
-- Test Cases
-----------------------------
-- TODO: Add test cases.

-- Tests to verify TVar update for
-- addUsers :: RoomName
addUsers = do
  state <- newTVarIO Server.emptyStore
  Server.createRoom state "base"
  Server.addUserToRoom state "user" "base"
  Server.getUserRoom state "user"

-- Adding users to room (initially)
testAddingUsers :: Test
testAddingUsers =
  "adding users"
    ~: TestList [addUsers ~?= "base"]

-- Switching users to other rooms

-- Adding users to threads

-- Sending users back to the same room

-- Sending messages in rooms
