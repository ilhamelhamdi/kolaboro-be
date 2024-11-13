{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Auth.Login (LoginAPI, loginHandler) where

import Control.Exception ()
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON)
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import Database.PostgreSQL.Simple
import GHC.Generics (Generic)
import Model.User (User (..))
import Servant
import Servant.Auth.Server
import Utils.JWTUtils (generateToken)
import Data.Time (UTCTime)

type LoginAPI = "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] Token

newtype Token = Token {unToken :: Text}
  deriving (Eq, Show, Generic)

instance ToJSON Token

data LoginRequest = LoginRequest
  { email :: String,
    password :: String
  }
  deriving (Eq, Show, Generic)

instance FromJSON LoginRequest

instance ToJSON LoginRequest

loginHandler :: JWTSettings -> Pool Connection -> LoginRequest -> Handler Token
loginHandler jwtSettings pool (LoginRequest _email _password) = do
  user <- liftIO $ validateUser pool _email _password
  case user of
    Just u -> do
      token <- liftIO $ generateToken jwtSettings u
      case token of
        Left _ -> throwError $ err401 {errBody = "Failed to generate token"}
        Right t -> return $ Token t
    Nothing ->
      throwError err401 {errBody = "Invalid credentials"}

validateUser :: Pool Connection -> String -> String -> IO (Maybe User)
validateUser pool _email _password = do
  liftIO $ withResource pool $ \conn -> do
    let q = "SELECT email, password, registered_date FROM users WHERE email=? AND password=?"
    users <- query conn q (_email, _password) :: IO [(String, String, UTCTime)]
    case users of
      [] -> return Nothing
      _ -> do
        let (_email, _name, _registered_date) = head users
        return $ Just $ User _email "" _name _registered_date