{-# LANGUAGE TypeOperators #-}

module API.Root (API) where

import API.Auth.Main (AuthAPI)
import API.Canvas.Main (CanvasAPI)
import API.Note.Main (NoteAPI)
import API.Profile.Main (ProfileAPI)
import API.Protected (ProtectedAPI)
import API.Stream.Main (StreamApi)
import Servant

type API =
  AuthAPI
    :<|> ProtectedAPI
    :<|> CanvasAPI
    :<|> NoteAPI
    :<|> StreamApi
    :<|> ProfileAPI
