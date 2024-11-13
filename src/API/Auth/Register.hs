{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module API.Auth.Register (RegisterAPI, registerHandler) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON)
import Data.Pool (Pool, withResource)
import Data.Time (UTCTime, getCurrentTime)
import Database.PostgreSQL.Simple (Connection, Only (Only), query)
import GHC.Generics (Generic)
import Model.User
import qualified Model.User as User
import Servant
import Prelude hiding (id)

type RegisterAPI = "register" :> ReqBody '[JSON] RegisterRequest :> Post '[JSON] NoContent

data RegisterRequest = RegisterRequest
  { email :: String,
    password :: String,
    username :: String,
    display_name :: String
  }
  deriving (Eq, Show, Generic)

instance FromJSON RegisterRequest

instance ToJSON RegisterRequest

registerHandler :: Pool Connection -> RegisterRequest -> Handler NoContent
registerHandler pool (RegisterRequest email password username display_name) = do
  registeredUser <- liftIO $ queryUser pool email
  case registeredUser of
    Just _ -> throwError err401 {errBody = "User already registered"}
    Nothing -> return ()
  currentTime <- liftIO getCurrentTime
  let user = User {email, password, username, display_name, id = 0, registered_at = currentTime, modified_at = currentTime}
  savedUser <- liftIO $ saveUser pool user
  case savedUser of
    Just _ -> return NoContent
    Nothing -> throwError err401 {errBody = "Failed to register user"}

queryUser :: Pool Connection -> String -> IO (Maybe User)
queryUser pool email = do
  liftIO $ withResource pool $ \conn -> do
    let q = "SELECT * FROM users WHERE email=?"
    users <- query conn q (Only email) :: IO [UserTuple]
    case users of
      [] -> return Nothing
      _ -> do
        return $ Just (fromTuple $ head users)

saveUser :: Pool Connection -> User -> IO (Maybe User)
saveUser pool user = do
  liftIO $ withResource pool $ \conn -> do
    let q = "INSERT INTO users (email, password, username, display_name, registered_at, modified_at) VALUES (?, ?, ?, ?, ?, ?) RETURNING *"
    users <- query conn q (toTupleWithoutId user) :: IO [UserTuple]
    case users of
      [] -> return Nothing
      _ -> do
        return $ Just $ User.fromTuple $ head users
  where
    toTupleWithoutId :: User -> (String, String, String, String, UTCTime, UTCTime)
    toTupleWithoutId User {..} = (email, password, username, display_name, registered_at, modified_at)