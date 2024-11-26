{-# LANGUAGE TypeOperators #-}

module API.Root (API) where

import API.Auth.Main (AuthAPI)
import API.Protected (ProtectedAPI)
import API.User (UserAPI)
import Servant
import API.Canvas.Main (CanvasAPI)

type API =
  UserAPI
    :<|> AuthAPI
    :<|> ProtectedAPI
    :<|> CanvasAPI
