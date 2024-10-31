{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.User (UserAPI, userServer) where

import Model.User (User)
import Servant
import Dummy (dummyUsers)

type UserAPI = "users" :> Get '[JSON] [User]

userServer :: Handler [User]
userServer = return dummyUsers