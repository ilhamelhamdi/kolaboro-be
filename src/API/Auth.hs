{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Auth (AuthAPI, authServer) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON)
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Connection, query)
import Dummy (dummyToken)
import GHC.Generics (Generic)
import Model.User hiding (email, password)
import Servant
import Servant.Auth.Server (JWTSettings)
import Utils.JWTUtils (generateToken)

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

type AuthAPI =
  "auth"
    :> ( "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] Token
           :<|> "register" :> ReqBody '[JSON] User :> Post '[JSON] Token
       )

loginHandler :: JWTSettings -> Pool Connection -> LoginRequest -> Handler Token
loginHandler jwtSettings pool (LoginRequest _email _password) = do
  user <- liftIO $ queryUser pool _email _password
  case user of
    Just u -> do
      token <- liftIO $ generateToken jwtSettings u
      case token of
        Left _ -> throwError $ err401 {errBody = "Failed to generate token"}
        Right t -> return $ Token t
    Nothing ->
      throwError err401 {errBody = "Invalid credentials"}

-- TODO : Implement registration
registerHandler :: User -> Handler Token
registerHandler _user = return $ Token dummyToken

authServer :: JWTSettings -> Pool Connection -> Server AuthAPI
authServer jwtSettings pool = loginHandler jwtSettings pool :<|> registerHandler

queryUser :: Pool Connection -> String -> String -> IO (Maybe User)
queryUser pool _email _password = do
  liftIO $ withResource pool $ \conn -> do
    let q = "SELECT email, password, registered_date FROM users WHERE email=? AND password=?"
    users <- query conn q (_email, _password) :: IO [(String, String, UTCTime)]
    case users of
      [] -> return Nothing
      _ -> do
        let (_email, _name, _registered_date) = head users
        return $ Just $ User _email "" _name _registered_date
