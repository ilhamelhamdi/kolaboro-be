{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Auth.Register (RegisterAPI, registerHandler) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON)
import Data.Pool (Pool, withResource)
import Data.Time (UTCTime, getCurrentTime)
import Database.PostgreSQL.Simple (Connection, Only (Only), query)
import GHC.Generics (Generic)
import Model.User hiding (email, password)
import Servant

type RegisterAPI = "register" :> ReqBody '[JSON] RegisterRequest :> Post '[JSON] NoContent

data RegisterRequest = RegisterRequest
  { email :: String,
    password :: String,
    name :: String
  }
  deriving (Eq, Show, Generic)

instance FromJSON RegisterRequest

instance ToJSON RegisterRequest

registerHandler :: Pool Connection -> RegisterRequest -> Handler NoContent
registerHandler pool (RegisterRequest _email _password _name) = do
  user <- liftIO $ queryUser pool _email
  case user of
    Just _ -> throwError err401 {errBody = "User already registered"}
    Nothing -> return ()
  registeredTime <- liftIO getCurrentTime
  let _user = User _email _password _name registeredTime
  savedUser <- liftIO $ saveUser pool _user
  case savedUser of
    Just _ -> return NoContent
    Nothing -> throwError err401 {errBody = "Failed to register user"}

queryUser :: Pool Connection -> String -> IO (Maybe User)
queryUser pool _email = do
  liftIO $ withResource pool $ \conn -> do
    let q = "SELECT email, name, registered_date FROM users WHERE email=?"
    users <- query conn q (Only _email) :: IO [(String, String, UTCTime)]
    case users of
      [] -> return Nothing
      _ -> do
        let (_email, _name, _registered_date) = head users
        return $ Just $ User _email "" _name _registered_date

saveUser :: Pool Connection -> User -> IO (Maybe User)
saveUser pool (User _email _password _userName _registeredDate) = do
  liftIO $ withResource pool $ \conn -> do
    let q = "INSERT INTO users (email, password, name, registered_date) VALUES (?, ?, ?, ?) RETURNING email, name, registered_date"
    users <- query conn q (_email, _password, _userName, _registeredDate) :: IO [(String, String, UTCTime)]
    case users of
      [] -> return Nothing
      _ -> do
        let (_email, _name, _registered_date) = head users
        return $ Just $ User _email "" _name _registered_date