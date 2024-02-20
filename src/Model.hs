{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Model where

import Data.Text
import Brick.Forms
import qualified Brick.Widgets.List as L
import Lens.Micro.TH
import Lens.Micro

type RName = String

type CustomEvent = ()

data Model = Model {
      exampleText :: String
    , _listOfStatements :: L.List RName Text
    , _myForm :: Form StringData CustomEvent RName
    , _currentTextAction :: TextAction
    }

data TextAction = MkTextAction {
  prompt :: Text
, action :: Model -> Model
}

data StringData = MkStringData {
    _innerData :: Text
}

makeLenses ''StringData
makeLenses ''Model

-- Utils

resetInput :: Model -> Model
resetInput = myForm %~ updateFormState (MkStringData "")

inputStatement :: Model -> Text
inputStatement = (^. innerData) . formState . (^. myForm)

addToStatementList :: Text -> Model -> Model
addToStatementList statement = listOfStatements %~ L.listInsert 0 statement