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
import Lens.Micro
import qualified Brick.Widgets.List as L

type RName = String

type CustomEvent = ()

data Model = Model {
      exampleText :: String
    , _listOfStatements :: L.List RName Text
    , _myForm :: Form StringData CustomEvent RName
    , _currentTextAction :: TextAction
    }

data TextAction = MkTextAction { prompt :: Text
                               , action :: Model -> Model}

data StringData = MkStringData {
    _innerData :: Text
}

makeLenses ''StringData
makeLenses ''Model

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
    where initialAction = MkTextAction { prompt =  "Please type in a username to grant them access to the database"
                                       , action = addGrantAllMessage}

addGrantAllMessage :: Model -> Model
addGrantAllMessage = (currentTextAction .~ newAction)
                   . resetInput
                   . \model -> addToStatementList ("GRANT ALL TO " <> inputStatement model <> " DUDE, IT'LL BE FINE") model
  where
    newAction = askForPassword

resetInput :: Model -> Model
resetInput = myForm %~ updateFormState (MkStringData "")

askForPassword :: TextAction
askForPassword = MkTextAction {
  prompt = "Please type in your secure password"
, action = \model -> addToStatementList ("USE PASSWORD " <> inputStatement model <> " FOR THE GRANT") model
}

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


addToStatementList :: Text -> Model -> Model
addToStatementList statement = listOfStatements %~ L.listInsert 0 statement

addInputToList :: Model -> Model
addInputToList model = model
                     & listOfStatements %~ L.listInsert 0 (inputStatement model)
                     & myForm %~ updateFormState (MkStringData "")
                     
inputStatement :: Model -> Text
inputStatement = (^. innerData) . formState . (^. myForm)
               

appEvent :: T.BrickEvent RName e -> T.EventM RName Model ()
appEvent (T.VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> do M.halt
        -- V.EvKey (V.KChar 'p') [] -> modify $ \model -> model {exampleText = "Peter"}
        -- V.EvKey (V.KChar 'r') [] -> modify $ \model -> model {exampleText = "Randy"}
        V.EvKey V.KEnter [] -> modify $ \model -> (action . _currentTextAction) model model
        _ -> T.zoom myForm (handleFormEvent $ T.VtyEvent e)