module Server2 where

import qualified Control.Monad as Monad
import Data.List (delete)
import Data.Map (Map)
import qualified Data.Map as Map
-- import Data.Text (strip)
import State (State)
import qualified State as S

-----------------------------
-- Type Definitions (Model)
-----------------------------

type RoomName = String

type User = String

type MessageContent = String

type Store = Map RoomName Room

data Room = R {name :: String, messages :: [Message], users :: [User]}

data Message = M {sender :: User, content :: String} deriving (Show)

emptyStore :: Store
emptyStore = Map.empty

-----------------------------
-- Function Declarations
-----------------------------

getAllRooms :: Store -> [RoomName]
getAllRooms = Map.keys

getAllRoomMessages :: Store -> RoomName -> [Message]
getAllRoomMessages store room = case Map.lookup room store of
  Nothing -> []
  Just (R _ messages _) -> messages

createRoom :: RoomName -> State Store ()
createRoom r = do
  store <- S.get
  S.put (Map.insert r (R r [] []) store)

deleteRoom :: RoomName -> State Store ()
deleteRoom r = do
  store <- S.get
  S.put (Map.delete r store)

addUserToRoom :: User -> RoomName -> State Store ()
addUserToRoom usr room = do
  store <- S.get
  S.put (Map.adjust updateRoom room store)
  where
    updateRoom rm@(R name messages users) =
      if usr `elem` users
        then rm
        else R name messages (usr : users)

removeUserFromRoom :: User -> RoomName -> State Store ()
removeUserFromRoom usr room = do
  store <- S.get
  S.put (Map.adjust (\(R name messages users) -> R name messages (delete usr users)) room store)

switchUserBetweenRooms :: User -> RoomName -> RoomName -> State Store ()
switchUserBetweenRooms usr r1 r2 = do
  removeUserFromRoom usr r1
  addUserToRoom usr r2

sendRoomMessage :: User -> RoomName -> MessageContent -> State Store ()
sendRoomMessage usr room msg = do
  store <- S.get
  S.put (Map.adjust updateRoom room store)
  where
    updateRoom rm@(R name messages users) =
      if usr `notElem` users
        then rm
        else R name (M usr msg : messages) users

-----------------------------
-- I/O
-----------------------------
-- evalState returns a value
-- execState returns store

run :: IO ()
run = go emptyStore
  where
    go :: Store -> IO ()
    go store = do
      putSpaceInfo store
      putStr "Client> "
      str <- getLine
      case str of
        -- add room
        'n' : ' ' : rm -> go $ S.execState (createRoom rm) store
        -- delete room
        'd' : ' ' : rm -> go $ S.execState (deleteRoom rm) store
        -- add user to room
        "u" -> go $ S.execState (addUserToRoom "bob" "test") store
        -- delete user from room
        "dd" -> go $ S.execState (removeUserFromRoom "bob" "test") store
        -- send message to room
        "m" -> go $ S.execState (sendRoomMessage "bob" "test" "hello room") store
        -- quit
        "x" -> return ()
        -- unknown command
        _ -> putStrLn "?" >> go store
    putSpaceInfo :: Store -> IO ()
    putSpaceInfo store = do
      print $ getAllRooms store
      print $ getAllRoomMessages store "test"