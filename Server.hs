module Server where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever, unless)
import Control.Monad.Fix (fix)
import qualified Data.ByteString.Char8 as C8
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

-----------------------------
-- Type Definitions
-----------------------------

type RoomName = String

type User = String

type MessageContent = String

type Response = String

type RoomStore = Map RoomName Room

type ThreadStore = Map Msg Thread

type UserStore = Map User (TChan Msg, Location)

type ServerState = TVar (RoomStore, ThreadStore, UserStore)

data Location
  = Room Room
  | Thread Thread

data Room = R
  { name :: String,
    messages :: [RoomMessage],
    users :: [User],
    channel :: TChan Msg
  }

data Thread = T
  { proom :: RoomName,
    pmsg :: Msg,
    tmessages :: [ThreadMessage],
    tusers :: [User],
    tchannel :: TChan Msg
  }

newtype ThreadMessage = TM {tmessage :: Msg} deriving (Eq)

data RoomMessage = RM {message :: Msg, thread :: Thread}

data Msg = M {sender :: User, content :: MessageContent} deriving (Eq, Ord)

instance Show ThreadMessage where
  show tm = sender (tmessage tm) ++ ": " ++ content (tmessage tm)

instance Show RoomMessage where
  show rm = sender (message rm) ++ ": " ++ content (message rm)

instance Show Msg where
  show msg = sender msg ++ ": " ++ content msg

emptyStore :: (RoomStore, ThreadStore, UserStore)
emptyStore = (Map.empty, Map.empty, Map.empty)

divider :: String
divider = "================================"

-----------------------------
-- State Logic Functions
-----------------------------

stmWrapper :: ServerState -> ((RoomStore, ThreadStore, UserStore) -> [String]) -> STM [String]
stmWrapper state pureFn = do
  store <- readTVar state
  return $ pureFn store

getAllRooms :: ServerState -> STM [RoomName]
getAllRooms state = do
  (roomStore, _, _) <- readTVar state
  return $ Map.keys roomStore

getAllRoomMessages :: ServerState -> RoomName -> STM [String]
getAllRoomMessages state room = do
  (roomStore, _, _) <- readTVar state
  return $ maybe [] (fmap show . messages) (Map.lookup room roomStore)

getAllThreadMessages :: ServerState -> RoomName -> Msg -> STM [String]
getAllThreadMessages state room msg = do
  (roomStore, threadStore, _) <- readTVar state
  case Map.lookup room roomStore of
    Just _ -> return $ threadParent msg threadStore
    Nothing -> return []
  where
    threadParent :: Msg -> ThreadStore -> [String]
    threadParent msg ts = maybe [] (fmap show . tmessages) (Map.lookup msg ts)

-- return $ maybe [] (fmap show . tmessages) (Map.lookup msg ts)

getUserChannel :: ServerState -> User -> STM (TChan Msg)
getUserChannel state user = do
  (_, _, userStore) <- readTVar state
  case Map.lookup user userStore of
    Nothing -> error $ "error: " ++ user ++ " has no channel."
    Just (tchan, _) -> return tchan

getUserRoom :: ServerState -> User -> STM RoomName
getUserRoom state user = do
  (_, _, userStore) <- readTVar state
  case Map.lookup user userStore of
    Nothing -> error $ "error: " ++ user ++ " has no channel."
    Just (_, loc) ->
      case loc of
        Room r -> return $ name r
        Thread t -> return $ proom t

createRoom :: ServerState -> RoomName -> STM [Response]
createRoom state roomName = do
  (roomStore, threadStore, userStore) <- readTVar state
  case Map.lookup roomName roomStore of
    Nothing -> do
      tchan <- newTChan
      let newRoom = R roomName [] [] tchan
      writeTVar state (Map.insert roomName newRoom roomStore, threadStore, userStore)
      return ["New room created!"]
    Just _ -> return ["Room already exists"]

createThread :: ServerState -> RoomName -> Msg -> STM [Response]
createThread state roomName roomMessage = do
  (roomStore, threadStore, userStore) <- readTVar state

  case Map.lookup roomMessage threadStore of
    Just _ -> return ["Thread already exists"]
    Nothing -> do
      tchan <- newTChan
      let newThread = T roomName roomMessage [] [] tchan
      writeTVar state (roomStore, Map.insert roomMessage newThread threadStore, userStore)
      return ["New thread created"]

-- | Add user to room user list and set the user's channel to a duplicate of the rooms channel.
addUserToRoom :: ServerState -> User -> RoomName -> STM [Response]
addUserToRoom state usr roomName = do
  (roomStore, threadStore, userStore) <- readTVar state
  case Map.lookup roomName roomStore of
    Nothing -> return []
    Just room -> do
      commLine <- dupTChan $ channel room
      let newRoom = updateRoom room
          newRoomStore = Map.insert roomName newRoom roomStore
          newUserStore = Map.insert usr (commLine, Room newRoom) userStore
      writeTVar state (newRoomStore, threadStore, newUserStore)
      getAllRoomMessages state roomName
  where
    updateRoom rm@(R name messages users channel) =
      if usr `elem` users
        then rm
        else R name messages (usr : users) channel

addUserToThread :: ServerState -> User -> RoomName -> Msg -> STM [Response]
addUserToThread state usr roomName rm = do
  (roomStore, threadStore, userStore) <- readTVar state
  case Map.lookup rm threadStore of
    Nothing -> return []
    Just thread -> do
      commLine <- dupTChan $ tchannel thread
      let newThread = updateThread thread
          newThreadStore = Map.insert rm newThread threadStore
          newUserStore = Map.insert usr (commLine, Thread newThread) userStore
      writeTVar state (roomStore, newThreadStore, newUserStore)
      getAllThreadMessages state roomName rm
  where
    updateThread t@(T pr pm tm tu tc) =
      if usr `elem` tu
        then t
        else T pr pm tm (usr : tu) tc

exitThread :: ServerState -> User -> RoomName -> STM [Response]
exitThread state usr rn = do
  (_, _, userStore) <- readTVar state
  case Map.lookup usr userStore of
    Nothing -> return []
    Just (_, Room _) -> return []
    Just (_, Thread _) -> addUserToRoom state usr rn

-- | Add message to room message list and room channel.
sendRoomMessage :: ServerState -> User -> RoomName -> MessageContent -> STM [Response]
sendRoomMessage state usr roomName msg = do
  (roomStore, _, _) <- readTVar state
  case Map.lookup roomName roomStore of
    Nothing -> return []
    Just (R name messages users channel) -> do
      -- create a new thread
      let message = M usr msg
      createThread state roomName message
      (roomStore, threadStore, userStore) <- readTVar state
      case Map.lookup message threadStore of
        Nothing -> return []
        Just t -> do
          let newRoom = R name (messages ++ [RM message t]) users channel
              newRoomStore = Map.insert roomName newRoom roomStore
          writeTChan channel message
          writeTVar state (newRoomStore, threadStore, userStore)
          return []

sendThreadMessage :: ServerState -> User -> RoomName -> MessageContent -> STM [Response]
sendThreadMessage state usr _ msg = do
  (roomStore, threadStore, userStore) <- readTVar state
  -- only called if user has a thread
  case Map.lookup usr userStore of
    Nothing -> return [] -- this should not be possible
    Just (_, Thread t) -> do
      case Map.lookup (pmsg t) threadStore of
        Nothing -> return []
        Just (T pr pm tm tu tc) -> do
          let message = M usr msg
          let newThread = T pr pm (tm ++ [TM message]) tu tc
              newThreadStore = Map.insert (pmsg t) newThread threadStore
          -- create a new thread
          writeTChan tc message
          writeTVar state (roomStore, newThreadStore, userStore)
          return []
    Just _ -> return []

sendMessage :: ServerState -> User -> RoomName -> MessageContent -> STM [Response]
sendMessage state usr rn msg = do
  (_, _, userStore) <- readTVar state
  case Map.lookup usr userStore of
    Just (_, Room _) -> sendRoomMessage state usr rn msg
    Just (_, Thread _) -> sendThreadMessage state usr rn msg
    Nothing -> return []

getRoomMessage :: ServerState -> RoomName -> Int -> STM [RoomMessage]
getRoomMessage state r i = do
  (roomStore, _, _) <- readTVar state
  case Map.lookup r roomStore of
    Nothing -> return []
    Just (R _ messages _ _) -> do
      let idx = length messages - i - 1
      return [messages !! idx]

-----------------------------
-- Server Logic Functions
-----------------------------

-- | Takes in ip address and creates + sets up the socket that listens for new connections.
setupConnSocket :: HostName -> IO Socket
setupConnSocket ip = do
  putStrLn "Opening connection socket..."
  addr <- getAddr
  connSock <- openSocket addr
  putStrLn "Connection socket has been setup."
  return connSock
  where
    getAddr = do
      let hints =
            defaultHints
              { addrFlags = [AI_PASSIVE],
                addrSocketType = Stream
              }
      addrInfos <- getAddrInfo (Just hints) (Just ip) (Just "5000")
      case addrInfos of
        [] -> error "Error getting address"
        (addrInfo : _) -> return addrInfo
    openSocket addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      return sock

-- | Loop that waits for new connection requests and spawns a new thread for each new connection.
connLoop :: Socket -> ServerState -> IO ()
connLoop connSock state = forever $ do
  (clientSock, clientAddr) <- accept connSock
  forkFinally (clientLoop clientSock clientAddr state) (const $ gracefulClose clientSock 5000)

-- | Handles client input.
handleInput :: ServerState -> User -> MessageContent -> STM [Response]
handleInput state usr msg = do
  roomName <- getUserRoom state usr
  case T.unpack (T.strip (T.pack msg)) of
    ':' : command -> case command of
      -- create room
      'n' : ' ' : rm -> do
        responses <- createRoom state rm
        return $ [divider] ++ responses ++ [divider]
      -- switch room
      's' : ' ' : rm -> do
        responses <- addUserToRoom state usr rm
        return $ ["clear", divider, "Switched Rooms!", divider] ++ responses
      -- switch to thread
      't' : ' ' : i -> do
        roomMessage <- getRoomMessage state roomName (read i)
        let msg = message (head roomMessage)
        responses <- addUserToThread state usr roomName msg
        return $ ["clear", divider, "Switched to Thread!", divider] ++ responses
      -- leave thread
      "b" -> do
        responses <- exitThread state usr roomName
        return $ ["clear", divider, "Left Thread!", divider] ++ responses
      -- get all rooms
      "g" -> do
        responses <- getAllRooms state
        return $ [divider, "Rooms:"] ++ responses ++ [divider]
      -- special call to get rooms to update room list, therefore it is formatted differently
      "gg" -> do
        responses <- getAllRooms state
        return $ "rooms" : responses
      -- unknown command
      _ -> return [divider, "Invalid Command!", divider]
    -- send message to room
    message -> sendMessage state usr roomName message

-- | Loop that handles a single client.
clientLoop :: Socket -> SockAddr -> ServerState -> IO ()
clientLoop clientSock clientAddr state = do
  putStrLn $ "Connected to client: " ++ show clientAddr
  let sendMsg msg = do
        -- putStrLn ("sending message: " ++ msg)
        sendAll clientSock (C8.pack msg)
      recvMsg = do
        byteStr <- recv clientSock 1024
        putStrLn ("received message: " ++ C8.unpack byteStr)
        return $ C8.unpack byteStr
      sendResponses user responses = sendMsg $ T.unpack $ T.strip $ T.pack $ unlines responses

  -- Get user name.
  user <- recvMsg
  let room = "base"

  responses <- atomically $ addUserToRoom state user room
  sendResponses user responses

  -- Thread to listen for new messages from the user's channel.
  reader <- forkIO $
    forever $ do
      commLine <- atomically $ getUserChannel state user
      isEmpty <- atomically $ isEmptyTChan commLine
      -- Only read from channel if not empty, then send message to client.
      unless isEmpty $ do
        nextMessage <- atomically $ readTChan commLine
        sendMsg $ show nextMessage

  -- Thread to listen for messages from the client.
  fix $ \loop -> do
    msg <- recvMsg
    case msg of
      ":q" -> sendMsg "Bye!"
      _ -> do
        responses <- atomically $ handleInput state user msg
        sendResponses user responses
        loop

  -- Cleanup.
  gracefulClose clientSock 5000
  killThread reader

main :: IO ()
main = do
  -- Get initial user input.
  putStrLn "What is your VPN IP address?"
  ip <- getLine

  -- Setup connection listener socket.
  connSocket <- setupConnSocket ip

  -- Initialize state.
  state <- newTVarIO emptyStore
  atomically $ createRoom state "base"

  -- Start main loop.
  connLoop connSocket state
