{-# LANGUAGE OverloadedStrings #-}
module Main where

import Model
import ControlFlow

import Brick.Main (defaultMain, App(..))
import Control.Monad.State (modify, liftIO)
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Border as BB
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Attributes as VA
import Data.Text (Text(..))
import qualified Data.Text as Text
import qualified Data.Vector as Vec
import qualified Data.Map as Map
import Brick.Forms
import Brick.Types (Widget)
import Brick.Widgets.Core
import Lens.Micro

main :: IO ()
main = do
    let initialState = Model {
      _windowText = "PROTOTYPE"
    , _listOfStatements = L.list "StatementList" (Vec.fromList ["-- Made with SQL Wizard"]) 1
    , _myForm = mkForm (MkStringData {_innerData = ""})
    , _currentTextAction = initialAction
    , _envVarMap = Map.empty
    , _currentUsername = Nothing
    , _currentPassword = Nothing
    }
    finalState <- defaultMain myApp initialState
    return ()
    where initialAction = ControlFlow.chooseDBProvider

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
    leftPanel = C.center $ txt $ "SQL Permissions Wizard " <> (model ^. windowText)
    rightPanel = L.renderList renderStatement False $ _listOfStatements model
    renderStatement _ statement = txt statement
    promptPanel = txt $ prompt $ _currentTextAction model

mkForm :: StringData -> Form StringData e RName
mkForm = newForm [editTextField innerData "Test String" (Just 3)]

appEvent :: T.BrickEvent RName e -> T.EventM RName Model ()
appEvent (T.VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> do 
          model <- T.get
          let sqlText = Text.unpack $ Text.intercalate "\n" $ Vec.toList $ L.listElements $ model ^. listOfStatements 
          liftIO $ writeFile "sql_wizard_output.sql" sqlText
          M.halt
        V.EvKey V.KEnter [] -> do
            model <- T.get
            newModel <- liftIO $ performAction model
            T.put newModel
        _ -> T.zoom myForm (handleFormEvent $ T.VtyEvent e)
  where 
    performAction :: Model -> IO Model
    performAction model =
           case model ^. currentTextAction of
             MkTextAction {action = f} -> return $ f model
             MkIOTextAction {ioAction = fio} -> fio model