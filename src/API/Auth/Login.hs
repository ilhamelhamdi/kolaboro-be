{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Auth.Login (LoginAPI, loginHandler) where

import Control.Exception ()
import Control.Monad.IO.Class (MonadIO (liftIO))
import DTO.ResponseDTO (ResponseDTO, jsonError401, successResponse)
import Data.Aeson (FromJSON, ToJSON)
import Data.Pool (Pool)
import Data.Text (Text)
import Database.PostgreSQL.Simple
import GHC.Generics (Generic)
import Model.User
import Repo.BaseRepo (BaseRepo (findByPredicate), PGRepo (..), Predicate (And), equals)
import Servant
import Servant.Auth.Server
import Utils.JWTUtils (generateToken)
import Prelude hiding (id)

type LoginAPI = "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] (ResponseDTO LoginResposeData)

data LoginRequest = LoginRequest
  { email :: String,
    password :: String
  }
  deriving (Eq, Show, Generic)

instance FromJSON LoginRequest

instance ToJSON LoginRequest

data LoginResposeData = LoginResposeData
  { token :: Text,
    user :: User
  }
  deriving (Generic)

instance ToJSON LoginResposeData

loginHandler :: JWTSettings -> Pool Connection -> LoginRequest -> Handler (ResponseDTO LoginResposeData)
loginHandler jwtSettings pool (LoginRequest _email _password) = do
  maybeUser <- liftIO $ validateUser pool _email _password
  case maybeUser of
    Just user_ -> do
      eitherToken <- liftIO $ generateToken jwtSettings user_
      case eitherToken of
        Left err -> throwError $ jsonError401 "Failed to login" (Just err)
        Right jwt -> do
          return $ successResponse "Logged in successful" $ Just (LoginResposeData jwt user_)
    Nothing ->
      throwError $ jsonError401 "Failed to login" (Just "Invalid credentials.")

validateUser :: Pool Connection -> String -> String -> IO (Maybe User)
validateUser pool reqEmail reqPassword = do
  liftIO $ findByPredicate (PGRepo pool :: PGRepo User) $ And (equals "email" reqEmail) (equals "password" reqPassword)