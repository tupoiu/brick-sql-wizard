{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Data.Text (Text(..))
import qualified Data.Vector as Vec
import Brick.Forms
import Brick.Types (Widget)
import Brick.Widgets.Core

import Lens.Micro.TH
import qualified Brick.Widgets.List as L

type RName = String

data Model e = Model {
      exampleText :: String
    , listOfStatements :: L.List RName Text
    , _myForm :: Form StringData e RName
    }

data StringData = MkStringData {
    _innerData :: Text
}

makeLenses ''StringData
makeLenses ''Model

main :: IO ()
main = do
    let initialState = Model {
      exampleText = "Hello"
    , listOfStatements = L.list "StatementList" (Vec.fromList ["My statement", "My second statement"]) 1
    , _myForm = mkForm (MkStringData {_innerData = "Test string 2"})
    }
        
    finalState <- defaultMain myApp initialState
    return ()

myApp :: App (Model e) e RName
myApp = App { M.appDraw = drawUI
            , M.appChooseCursor = M.showFirstCursor
            , M.appHandleEvent = appEvent
            , M.appStartEvent = return ()
            , M.appAttrMap = const (A.attrMap VA.defAttr [])
            }

drawUI :: Model e -> [Widget RName]
drawUI model = [(leftPanel <=> BB.hBorder <=> bottomLeft) <+> BB.vBorder <+> rightPanel]
  where
    bottomLeft = renderForm (_myForm model)
    leftPanel = C.center $ str $ "HELLO " <> exampleText model
    rightPanel = L.renderList renderStatement False $ listOfStatements model
    renderStatement _ statement = txt statement 

mkForm :: StringData -> Form StringData e RName
mkForm = newForm [editTextField innerData "Test String" (Just 3)]


addInputToList :: Model e -> Model e
addInputToList model = model { listOfStatements = L.listInsert 0 inputStatement (listOfStatements model)
                             , _myForm = updateFormState (MkStringData "") $ _myForm model
                             }
  where inputStatement = _innerData $ formState $ _myForm model

appEvent :: T.BrickEvent RName e -> T.EventM RName (Model e) ()
appEvent (T.VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> do M.halt
        V.EvKey (V.KChar 'p') [] -> modify $ \model -> model {exampleText = "Peter"}
        V.EvKey (V.KChar 'r') [] -> modify $ \model -> model {exampleText = "Randy"}
        V.EvKey V.KEnter [] -> modify $ addInputToList
        _ -> T.zoom myForm (handleFormEvent $ T.VtyEvent e)
