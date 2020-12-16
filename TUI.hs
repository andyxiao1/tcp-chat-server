{-# LANGUAGE TemplateHaskell #-}

module TUI
  ( app,
    initState,
    customMain,
    ServerResponse (..),
    BChan,
    newBChan,
    writeBChan,
  )
where

import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Forms
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Concurrent
import Control.Monad ()
import Control.Monad.Fix (fix)
import qualified Data.ByteString.Char8 as C8
import Data.List (isPrefixOf)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Lens.Micro.Platform (makeLenses, (^.))
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

-----------------------------
-- Type Definitions
-----------------------------

newtype UserInfo = FormState {_message :: T.Text} deriving (Show)

-- Don't move this line because it messes with the variable scoping.
makeLenses ''UserInfo

type RoomName = String

type User = String

type MessageContent = String

data ServerResponse = SR String | Tick

data ClientState = CS
  { rooms :: [RoomName],
    messages :: [MessageContent],
    user :: User,
    input :: Form UserInfo ServerResponse Name,
    sock :: Socket
  }

data Name = MessageView | RoomList | MessageInput deriving (Ord, Show, Eq)

-----------------------------
-- Constants / Initializers
-----------------------------

initUserInfo :: UserInfo
initUserInfo = FormState $ T.pack ""

theMap :: AttrMap
theMap = attrMap V.defAttr []

app :: App ClientState ServerResponse Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const theMap
    }

initState :: User -> Socket -> ClientState
initState user sock =
  CS
    { rooms = [],
      messages = [],
      user = user,
      input = mkForm initUserInfo,
      sock = sock
    }

mkForm :: UserInfo -> Form UserInfo e Name
mkForm = newForm [editTextField message MessageInput (Just 2)]

-----------------------------
-- Event Handler
-----------------------------

handleEvent :: ClientState -> BrickEvent Name ServerResponse -> EventM Name (Next ClientState)
-- Key Press Events.
handleEvent cs (VtyEvent (V.EvKey V.KEsc [])) = halt cs
handleEvent cs@(CS rooms messages user input sock) (VtyEvent (V.EvKey V.KEnter [])) = do
  let msg = T.unpack $ formState input ^. message
  if null msg
    then continue cs
    else do
      suspendAndResume $ do
        sendAll sock $ C8.pack msg
        return $ CS rooms messages user (mkForm initUserInfo) sock
-- Network Listener Events.
handleEvent (CS rooms messages user input sock) (AppEvent (SR resp))
  | "clear" `isPrefixOf` resp = do
    let remainingMsg = T.unpack $ T.strip $ T.pack $ drop 5 resp
        newMessages = ([remainingMsg | not (null remainingMsg)])
    continue $ CS rooms newMessages user input sock
  | "rooms" `isPrefixOf` resp = do
    let rooms = T.unpack $ T.strip $ T.pack $ drop 5 resp
    continue $ CS [rooms] messages user input sock
  | otherwise = continue $ CS rooms (resp : messages) user input sock
handleEvent cs (AppEvent Tick) =
  suspendAndResume $ do
    sendAll (sock cs) $ C8.pack ":gg"
    return cs
-- Form Events.
handleEvent (CS rooms messages user input sock) ev = do
  newInput <- handleFormEvent ev input
  continue $ CS rooms messages user newInput sock

-----------------------------
-- Drawing Functions
-----------------------------

drawUI :: ClientState -> [Widget Name]
drawUI cs =
  [ C.center $
      B.border $
        drawRoomList cs
          <+> B.vBorder
          <+> (drawMessageView cs <=> B.hBorder <=> drawMessageInput cs)
  ]

drawRoomList :: ClientState -> Widget Name
drawRoomList cs =
  hLimit 25 $
    viewport RoomList Vertical $
      C.hCenter (str "ROOMS")
        <=> drawStrList (rooms cs)

drawMessageView :: ClientState -> Widget Name
drawMessageView cs =
  viewport MessageView Vertical $
    C.hCenter (str "MESSAGES")
      <=> drawStrList (messages cs)

drawStrList :: [String] -> Widget Name
drawStrList = foldr (\x acc -> acc <=> (padLeft (Pad 1) . str) x) B.hBorder

drawMessageInput :: ClientState -> Widget Name
drawMessageInput cs = padLeft (Pad 1) $ renderForm $ input cs