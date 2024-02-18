module Main where

import Brick.Main (defaultMain, App(..))
import Control.Monad.State (modify)
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Border as BB
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Attributes as VA
import Brick.Types (Widget)
import Brick.Widgets.Core
main :: IO ()
main = do
    let initialState = Model {exampleText = "Hello"}
    finalState <- defaultMain myApp initialState
    return ()

type RName = String

myApp :: App Model e RName
myApp = App { M.appDraw = drawUI
            , M.appChooseCursor = M.showFirstCursor
            , M.appHandleEvent = appEvent
            , M.appStartEvent = return ()
            , M.appAttrMap = const (A.attrMap VA.defAttr [])
            }

drawUI :: Model -> [Widget RName]
drawUI model = [(leftPanel <=> BB.hBorder <=> bottomLeft) <+> BB.vBorder <+> rightPanel]
  where
    bottomLeft = C.center $ str "Bottomleft"
    leftPanel = C.center $ str $ "HELLO " <> exampleText model
    rightPanel = C.center $ str $ "This is my terminal app"

data Model = Model {exampleText :: String}

appEvent :: T.BrickEvent RName e -> T.EventM RName Model ()
appEvent (T.VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> do M.halt
        V.EvKey (V.KChar 'p') [] -> modify $ \model -> model {exampleText = "Peter"}
        V.EvKey (V.KChar 'r') [] -> modify $ \model -> model {exampleText = "Randy"}
        _ -> return ()