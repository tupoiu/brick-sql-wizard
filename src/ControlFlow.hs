{-# LANGUAGE OverloadedStrings #-}
module ControlFlow where

import Model

import Lens.Micro
import Data.Text (Text(..))
import Data.String (IsString)
import Text.Printf(printf)
import qualified Data.Text as Text
import Data.List (intercalate)
import Data.Maybe (fromJust)
import System.Environment
import qualified Data.Map as Map

-- Control flow

compose = foldr (.) id

type Username = String
type Password = String

chooseEnvVars :: TextAction
chooseEnvVars = MkIOTextAction {
  prompt = "Please type in the EnvVars to look for on your machine, space separated"
, ioAction = act
}
  where 
    act :: Model -> IO Model
    act model = do
      (succs, misses) <- getEnvVars envVarNames
      return $ dealWithMissingEnvVars (succs, misses)
      where 
        envVarNames = words $ Text.unpack $ inputStatement model

        getEnvVars :: [String] -> IO ([(String,String)], [String])
        getEnvVars [] = return ([],[])
        getEnvVars (name:rest) = do
          maybeVar <- lookupEnv name
          (succs, misses) <- getEnvVars rest
          return $ case maybeVar of
            Just var -> ((name, var) : succs, misses)
            Nothing -> (succs, name : misses)

        dealWithMissingEnvVars :: ([(String,String)], [String]) -> Model
        dealWithMissingEnvVars (succs, misses) =
          case misses of
            [] -> model
              & (windowText .~ (Text.pack $ "Env vars: " ++ show succs))
              . (envVarMap .~ Map.fromList succs)
              . (currentTextAction .~ home)
              . resetInput

            _ -> resetInput $ (currentTextAction .~ (resetAction misses)) model

        resetAction :: [String] -> TextAction
        resetAction misses = MkIOTextAction {
          prompt = Text.pack $ "Failed to find Env vars: " <> intercalate ", " misses <> " try again"
        , ioAction = act
        }

home :: TextAction
home = MkTextAction {
  prompt = "Choose action: " <> Text.intercalate " | " ["[1] Create user", "[2] Revoke user", "[3] Change password"]
, action = act
}
  where
    act model = model & resetInput &
      case inputStatement model of
        "1" -> (currentTextAction .~ (getUsername $ getPassword $ createUserSQL))
        "2" -> (currentTextAction .~ (getUsername $ revokeUserSQL))
        "3" -> (currentTextAction .~ (getUsername $ getPassword $ changePassword))
        _ -> id

getUsername :: TextAction -> TextAction
getUsername nextAction = MkTextAction {
  prompt = "Please type a username"
, action = act
}
  where 
    act model = model & case inputStatement model of
      "" ->
        currentTextAction .~ 
        MkTextAction { prompt = "Username cannot be empty, try again"
                     , action = act}
      username | "postgres" `Text.isInfixOf` username ->
        currentTextAction .~ 
        MkTextAction { prompt = "Username cannot contain \"postgres\", try again"
                     , action = act}
      username ->
        (currentTextAction .~ nextAction)
        . (currentUsername .~ Just (inputStatement model))
        . resetInput

getPassword :: TextAction -> TextAction
getPassword nextAction = MkTextAction {
  prompt = "Please type a password"
, action = act
}
  where 
    act model = model & case inputStatement model of
      "" ->
        currentTextAction .~ 
        MkTextAction { prompt = "Password cannot be empty, try again"
                     , action = act}
      password | Text.length password < 9 ->
        currentTextAction .~ 
        MkTextAction { prompt = "Password must be at least 9 characters, try again"
                     , action = act}
      password ->
        (currentTextAction .~ nextAction)
        . (currentPassword .~ Just (inputStatement model))
        . resetInput

createUserSQL :: TextAction
createUserSQL = MkTextAction {
  prompt = "Create user? (y/n)"
, action = act
} 
  where
    nextAction = home
    dbs = ["testDB1", "testDB2"]
    act model = model & case inputStatement model of
        "y" ->
            (compose $ map (addToStatementList . Text.pack) statements)
          . resetInput
          . (currentTextAction .~ nextAction)
          where
            statements = [createUserStatement] ++ map connectGrant dbs ++ map usageGrant dbs
            createUserStatement = printf "CREATE USER %s WITH PASSWORD '%s';" usr pwd
            connectGrant :: String -> String
            connectGrant db = printf "GRANT CONNECT ON DATABASE %s TO %s;" db usr
            usageGrant :: String -> String
            usageGrant db =
              printf "\\c %s\n" db
                ++ "\n"
                ++ printf "GRANT USAGE ON SCHEMA public TO %s;" usr
                ++ "\n"
                ++ printf "GRANT SELECT ON ALL TABLES IN SCHEMA public TO %s;" usr
                
        "n" ->
            resetInput
          . (currentTextAction .~ nextAction)
        _ ->
          currentTextAction .~ 
          MkTextAction { prompt = "Please type 'y' or 'n'. Create user?"
                       , action = act}
        where 
          usr = Text.unpack $ fromJust (model ^. currentUsername)
          pwd = Text.unpack $ fromJust (model ^. currentPassword)

revokeUserSQL :: TextAction
revokeUserSQL = MkTextAction {
  prompt = "Type the databases for which you want to revoke access"
, action = act
}
  where
    nextAction = home
    act model = model
        & compose (map (addToStatementList . Text.pack) statements)
        . resetInput
        . (currentTextAction .~ nextAction)
            where
              usr = Text.unpack $ fromJust (model ^. currentUsername)
              pwd = Text.unpack $ fromJust (model ^. currentPassword)
              dbs = filter (not . null) $ words $ Text.unpack $ inputStatement model
              statements = map revokeDbGrant dbs ++ [revokeSchemaPublic, dropUserStatement]
              revokeDbGrant db = printf "REVOKE ALL ON DATABASE %s FROM %s;\n" db usr
              revokeSchemaPublic = printf "REVOKE ALL ON SCHEMA public FROM %s;\n" usr
              dropUserStatement = printf "DROP USER %s;\n" usr

changePassword :: TextAction
changePassword = MkTextAction {
  prompt = "Change password? (y/n)"
, action = act
}
  where
    act model = model
        & (addToStatementList $ Text.pack changePasswordStatement)
        . resetInput
        . (currentTextAction .~ home)
          where
            usr = Text.unpack $ fromJust (model ^. currentUsername)
            pwd = Text.unpack $ fromJust (model ^. currentPassword)
            changePasswordStatement = printf "ALTER USER %s WITH PASSWORD '%s';" usr pwd