{-# LANGUAGE OverloadedStrings #-}
module ControlFlow where

import Model

import Lens.Micro
import Data.Text (Text(..))

-- Control flow

addGrantAllMessage :: TextAction
addGrantAllMessage = MkTextAction{ 
  prompt = "Please type in a username to grant them access to the database"
, action = 
    (currentTextAction .~ newAction)
    . resetInput
    . \model -> addToStatementList ("GRANT ALL TO " <> inputStatement model <> " DUDE, IT'LL BE FINE") model
} where
    newAction = askForPassword

askForPassword :: TextAction
askForPassword = MkTextAction {
  prompt = "Please type in your secure password"
, action = (currentTextAction .~ newAction)
    . resetInput
    . \model -> addToStatementList ("USE PASSWORD " <> inputStatement model <> " FOR THE GRANT") model
} where
    newAction = MkTextAction {
  prompt = "Fin"
, action = id
}