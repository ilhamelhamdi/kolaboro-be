{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Auth.Main (AuthAPI, authServer) where

import API.Auth.Login
import API.Auth.Register (RegisterAPI, registerHandler)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Servant.Auth.Server (JWTSettings)

type AuthAPI = "auth" :> (LoginAPI :<|> RegisterAPI)

authServer :: JWTSettings -> Pool Connection -> Server AuthAPI
authServer jwtSettings pool = loginHandler jwtSettings pool :<|> registerHandler pool
