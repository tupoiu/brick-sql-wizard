{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Model where

import Data.Text (Text(..))
import Data.String (IsString)
import Brick.Forms
import qualified Brick.Widgets.List as L
import Lens.Micro.TH
import Lens.Micro
import Data.Map (Map(..))

type RName = String

type CustomEvent = ()

data Model = Model {
      _windowText :: Text
    , _listOfStatements :: L.List RName Text
    , _myForm :: Form StringData CustomEvent RName
    , _currentTextAction :: TextAction
    , _envVarMap :: Map String String
    , _currentUsername :: Maybe Text
    , _currentPassword :: Maybe Text
    }

data TextAction = MkTextAction {
  prompt :: Text
, action :: Model -> Model
} | MkIOTextAction {
  prompt :: Text
, ioAction :: Model -> IO Model
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
addToStatementList statement = listOfStatements %~ \list ->
   L.listInsert (length list) statement list