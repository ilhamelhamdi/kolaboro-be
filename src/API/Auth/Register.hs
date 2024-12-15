{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module API.Auth.Register (RegisterAPI, registerHandler) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import DTO.ResponseDTO (jsonError)
import Data.Aeson (FromJSON, ToJSON)
import Data.Pool (Pool)
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import Model.User
import Repo.BaseRepo (BaseRepo (create, findByPredicate), PGRepo (PGRepo), equals)
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
  let connPool = PGRepo pool :: PGRepo User
  registeredUser <- liftIO $ findByPredicate connPool $ equals "email" email
  case registeredUser of
    Just _ ->
      throwError $ jsonError err409 "Register Failed" (Just "User already registered.")
    Nothing -> return ()
  currentTime <- liftIO getCurrentTime
  let user = User {email, password, username, display_name, id = 0, registered_at = currentTime, modified_at = currentTime}
  _ <- liftIO $ create connPool user
  return NoContent