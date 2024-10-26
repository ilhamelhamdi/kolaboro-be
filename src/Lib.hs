{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib (startApp) where

import Data.Aeson (FromJSON)
import Dummy (dummyToken, dummyUsers)
import GHC.Generics (Generic)
import Model.User
import Network.Wai.Handler.Warp (run)
import Servant

data LoginDTO = LoginDTO
  { username :: String,
    password :: String
  }
  deriving (Eq, Show, Generic)

instance FromJSON LoginDTO

type Token = String

type UserAPI = "users" :> Get '[JSON] [User]

type AuthAPI =
  "auth"
    :> ( "login" :> ReqBody '[JSON] LoginDTO :> Post '[JSON] Token
           :<|> "register" :> ReqBody '[JSON] User :> Post '[JSON] Token
       )

type API = UserAPI :<|> AuthAPI

userServer :: Handler [User]
userServer = return dummyUsers

authServer :: (LoginDTO -> Handler Token) :<|> (User -> Handler Token)
authServer = loginHandler :<|> registerHandler
  where
    loginHandler _loginDTO = return dummyToken
    registerHandler _user = return dummyToken

server :: Server API
server = userServer :<|> authServer

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

startApp :: IO ()
startApp = run 8081 app