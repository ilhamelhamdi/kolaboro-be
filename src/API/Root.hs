{-# LANGUAGE TypeOperators #-}

module API.Root (API) where

import API.Auth.Main (AuthAPI)
import API.Protected (ProtectedAPI)
import API.User (UserAPI)
import Servant
import API.Canvas.Main (CanvasAPI)
import API.Stream.Main (StreamApi)
import API.Note.Main (NoteAPI)

type API =
  UserAPI
    :<|> AuthAPI
    :<|> ProtectedAPI
    :<|> CanvasAPI
    :<|> NoteAPI
    :<|> StreamApi
