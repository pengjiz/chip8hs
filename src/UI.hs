{-# LANGUAGE OverloadedStrings #-}

module UI (Event (..), app) where

import           Brick                (App (..), AttrMap, AttrName,
                                       BrickEvent (..), EventM, Next, Widget,
                                       attrMap, cached, continue, emptyWidget,
                                       hBox, hLimit, halt, invalidateCacheEntry,
                                       neverShowCursor, str, strWrap, vBox,
                                       withAttr)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty         as T
import           Lens.Micro.Platform  (each, ix, preview, set, toListOf, view)
import           Machine              (Error, Machine, decayKeypad, decayTimers,
                                       dirty, fb, keypad, pixels, pressKey,
                                       screenWidth, st, step)
import           Text.Printf          (printf)
import           Util                 (chunksOf)

-- | Application state.
type State = Either Error Machine

-- | Application resource name.
data Name =
  -- | Screen area.
  Screen
  deriving (Eq, Ord)

-- | Application custom event.
data Event =
  -- | Execute one step.
  Execute
  -- | Redraw screen.
  | Redraw
  -- | Decrement delay and sound timers.
  | DecayTimers
  -- | Decrement keypad values.
  | DecayKeypad

-- | Application.
app :: App State Event Name
app = App { appDraw = draw
          , appHandleEvent = handleEvent
          , appAttrMap = const theme
          , appStartEvent = pure
          , appChooseCursor = neverShowCursor
          }

-- | Draw UI.
draw :: State -> [Widget Name]
draw s = [C.center $ hBox [drawScreen s, drawInfo s]]

-- | Draw screen.
drawScreen :: State -> Widget Name
drawScreen (Left e) = B.borderWithLabel (str "Error") $ strWrap (show e)
drawScreen (Right m) = B.borderWithLabel (str "Screen")
  $ cached Screen
  $ vBox rows
  where ps = toListOf (fb . pixels . each) m
        rows = map drawRow . chunksOf screenWidth $ ps
        drawRow = hBox . map drawPixel
        drawPixel p = let a = if p then pixelOnAttr else pixelOffAttr
                      in withAttr a $ str "  "

-- | Draw other information.
drawInfo :: State -> Widget Name
drawInfo (Left _)  = emptyWidget
drawInfo (Right m) = hLimit 20 $ vBox [drawSound m, drawKeypad m]

-- | Draw sound information.
drawSound :: Machine -> Widget Name
drawSound m = B.borderWithLabel (str "Sound")
  $ C.hCenter
  $ str (if view st m > 0 then "**" else "--")

-- | Draw keypad.
drawKeypad :: Machine -> Widget Name
drawKeypad m = B.borderWithLabel (str "Keypad")
  $ C.hCenter
  $ vBox rows
  where rows = drawRow <$> keypadLayout
        drawRow = hBox . map drawKey
        drawKey n = case preview (keypad . ix n) m of
          Nothing -> str " - "
          Just v  -> let a = if v > 0 then keyDownAttr else keyUpAttr
                     in withAttr a . str $ printf " %X " n

-- | Layout of key codes on the keypad.
keypadLayout :: [[Int]]
keypadLayout = [ [0x1, 0x2, 0x3, 0xc]
               , [0x4, 0x5, 0x6, 0xd]
               , [0x7, 0x8, 0x9, 0xe]
               , [0xa, 0x0, 0xb, 0xf]
               ]

-- | Handle event.
handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent s@(Right m) (AppEvent e) = case e of
  Execute     -> continue $ step m
  Redraw      -> if view (fb . dirty) m
                 then invalidateCacheEntry Screen >>
                      (continue . pure $ set (fb . dirty) False m)
                 else continue s
  DecayTimers -> continue . pure $ decayTimers m
  DecayKeypad -> continue . pure $ decayKeypad m
handleEvent s@(Right m) (VtyEvent (T.EvKey k [])) = case k of
  (T.KChar '1') -> g 0x1
  (T.KChar '2') -> g 0x2
  (T.KChar '3') -> g 0x3
  (T.KChar '4') -> g 0xc
  (T.KChar 'q') -> g 0x4
  (T.KChar 'w') -> g 0x5
  (T.KChar 'e') -> g 0x6
  (T.KChar 'r') -> g 0xd
  (T.KChar 'a') -> g 0x7
  (T.KChar 's') -> g 0x8
  (T.KChar 'd') -> g 0x9
  (T.KChar 'f') -> g 0xe
  (T.KChar 'z') -> g 0xa
  (T.KChar 'x') -> g 0x0
  (T.KChar 'c') -> g 0xb
  (T.KChar 'v') -> g 0xf
  (T.KChar 'k') -> halt s
  T.KEsc        -> halt s
  _             -> continue s
  where g = continue . (`pressKey` m)
handleEvent e@(Left _) (VtyEvent (T.EvKey _ _)) = halt e
handleEvent s _ = continue s

-- | Widget theme.
theme :: AttrMap
theme = let r = T.currentAttr `T.withStyle` T.reverseVideo
        in attrMap T.defAttr [ (pixelOnAttr, r)
                             , (keyDownAttr, r)
                             ]

-- | Attribute name for an on pixel.
pixelOnAttr :: AttrName
pixelOnAttr = "pixelOn"

-- | Attribute name for an off pixel.
pixelOffAttr :: AttrName
pixelOffAttr = "pixelOff"

-- | Attribute name for a pressed key.
keyDownAttr :: AttrName
keyDownAttr = "keyDown"

-- | Attribute name for an unpressed key.
keyUpAttr :: AttrName
keyUpAttr = "keyUp"
