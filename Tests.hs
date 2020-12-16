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
main =
  tAddUsers
    >> tSwitchUsers
    >> tAddUserToThread
    >> tAddUserToRoomAfterThread
    >> tSendMessages

------------------------- Test cases for the interpreter -----
-----------------------------
-- Test Cases
-----------------------------
-- TODO: Add test cases.

-- Tests to verify TVar update for

-- Adding users to room (initially)
tAddUsers :: IO ()
tAddUsers = do
  state <- newTVarIO Server.emptyStore
  resp <- atomically $ Server.createRoom state "base"
  resp <- atomically $ Server.addUserToRoom state "user" "base"
  resp <- atomically $ Server.getUserRoom state "user"
  putStrLn ("Received response: " ++ resp)
  putStrLn ("Expected response: " ++ "base")
  if resp == "base"
    then putStrLn "Test passed"
    else putStrLn "Test failed"

-- Switching users to other rooms

tSwitchUsers :: IO ()
tSwitchUsers = do
  state <- newTVarIO Server.emptyStore
  resp <- atomically $ Server.createRoom state "base"
  resp <- atomically $ Server.addUserToRoom state "user" "base"
  resp <- atomically $ Server.createRoom state "room2"
  resp <- atomically $ Server.addUserToRoom state "user" "room2"
  resp <- atomically $ Server.getUserRoom state "user"
  putStrLn ("Received response: " ++ resp)
  putStrLn ("Expected response: " ++ "room2")
  if resp == "room2"
    then putStrLn "Test passed"
    else putStrLn "Test failed"

-- Adding users to threads
tAddUserToThread :: IO ()
tAddUserToThread = do
  state <- newTVarIO Server.emptyStore
  resp <- atomically $ Server.createRoom state "base"
  resp <- atomically $ Server.addUserToRoom state "user" "base"
  resp <- atomically $ Server.sendRoomMessage state "user" "base" "hello"
  resp <- atomically $ Server.addUserToThread state "user" "base" (M "user" "hello")
  (_, _, userStore) <- readTVarIO state
  case Map.lookup "user" userStore of
    Just (_, Room r) -> putStrLn "User is not in thread\nTest Failed"
    Just (_, Thread t) -> putStrLn "Added user to thread\nTest Passed"
    Nothing -> putStrLn "User is not in thread\nTest Failed"

-- Sending users back to the same room

tAddUserToRoomAfterThread :: IO ()
tAddUserToRoomAfterThread = do
  state <- newTVarIO Server.emptyStore
  resp <- atomically $ Server.createRoom state "base"
  resp <- atomically $ Server.addUserToRoom state "user" "base"
  resp <- atomically $ Server.sendRoomMessage state "user" "base" "hello"
  resp <- atomically $ Server.addUserToThread state "user" "base" (M "user" "hello")
  resp <- atomically $ Server.exitThread state "user" "base"
  (_, _, userStore) <- readTVarIO state
  case Map.lookup "user" userStore of
    Just (_, Room r) -> putStrLn "User is not in thread\nTest Passed"
    Just (_, Thread t) -> putStrLn "User is still in thread\nTest Failed"
    Nothing -> putStrLn "User is not in thread\nTest Failed"

-- Sending messages in rooms

tSendMessages :: IO ()
tSendMessages = do
  state <- newTVarIO Server.emptyStore
  resp <- atomically $ Server.createRoom state "base"
  resp <- atomically $ Server.addUserToRoom state "user" "base"
  resp <- atomically $ Server.sendRoomMessage state "user" "base" "hello"
  resp <- atomically $ Server.sendRoomMessage state "user" "base" "how are you"
  resp <- atomically $ Server.getAllRoomMessages state "base"
  putStrLn ("Received " ++ show resp)
  putStrLn ("Expected " ++ "user: hello" ++ "user: how are you")
  if resp == ["user: hello", "user: how are you"]
    then putStrLn "Test passed"
    else putStrLn "Test failed"
