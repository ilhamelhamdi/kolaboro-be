{-# LANGUAGE TypeOperators #-}

module API.Root (API) where

import API.Auth (AuthAPI)
import API.User (UserAPI)
import Servant

type API = UserAPI :<|> AuthAPI
