{-# LANGUAGE OverloadedStrings #-}
module Main where

import Model
import ControlFlow

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
import Data.Text (Text(..))
import qualified Data.Vector as Vec
import Brick.Forms
import Brick.Types (Widget)
import Brick.Widgets.Core
import Lens.Micro

main :: IO ()
main = do
    let initialState = Model {
      exampleText = "Hello"
    , _listOfStatements = L.list "StatementList" (Vec.fromList ["My statement", "My second statement"]) 1
    , _myForm = mkForm (MkStringData {_innerData = ""})
    , _currentTextAction = initialAction
    }
    finalState <- defaultMain myApp initialState
    return ()
    where initialAction = ControlFlow.addGrantAllMessage

myApp :: App Model e RName
myApp = App { M.appDraw = drawUI
            , M.appChooseCursor = M.showFirstCursor
            , M.appHandleEvent = appEvent
            , M.appStartEvent = return ()
            , M.appAttrMap = const (A.attrMap VA.defAttr [])
            }

drawUI :: Model -> [Widget RName]
drawUI model = [(leftPanel <=> BB.hBorder <=> promptPanel <=> BB.hBorder <=> inputBox) <+> BB.vBorder <+> rightPanel]
  where
    inputBox = renderForm (_myForm model)
    leftPanel = C.center $ str $ "HELLO " <> exampleText model
    rightPanel = L.renderList renderStatement False $ _listOfStatements model
    renderStatement _ statement = txt statement
    promptPanel = txt $ prompt $ _currentTextAction model

mkForm :: StringData -> Form StringData e RName
mkForm = newForm [editTextField innerData "Test String" (Just 3)]
               
appEvent :: T.BrickEvent RName e -> T.EventM RName Model ()
appEvent (T.VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> do M.halt
        -- V.EvKey (V.KChar 'p') [] -> modify $ \model -> model {exampleText = "Peter"}
        -- V.EvKey (V.KChar 'r') [] -> modify $ \model -> model {exampleText = "Randy"}
        V.EvKey V.KEnter [] -> modify $ \model -> (action . _currentTextAction) model model
        _ -> T.zoom myForm (handleFormEvent $ T.VtyEvent e)