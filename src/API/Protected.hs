{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Protected where

import Data.Text (Text, pack)
import Servant hiding (BasicAuth)
import Servant.Auth (Auth, JWT)
import Servant.Auth.Server (AuthResult (..))
import Utils.JWTUtils (UserClaims(displayName))


type ProtectedAPI = Auth '[JWT] UserClaims :> "protected" :> Get '[JSON] Text

protectedHandler :: AuthResult UserClaims -> Handler Text
protectedHandler (Authenticated uClaim) = return $ "Hello, " <> pack (displayName uClaim)
protectedHandler _ = throwError err401 {errBody = "Unauthenticated"}