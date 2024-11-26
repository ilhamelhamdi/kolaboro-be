{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Canvas.Main (CanvasAPI, canvasServer) where

import API.Canvas.CreateCanvas (CreateCanvasAPI, createCanvasHandler)
import API.Canvas.GetCanvases (GetCanvasesAPI, getCanvasesHandler)
import Control.Monad.IO.Class (liftIO)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Model.User (User)
import Servant
import Servant.Auth.Server
import Utils.JWTUtils (UserClaims, getUserFromUserClaims)

type CanvasAPI = Auth '[JWT] UserClaims :> "canvas" :> (CreateCanvasAPI :<|> GetCanvasesAPI)

canvasServer :: Pool Connection -> Server CanvasAPI
canvasServer pool = authServer
  where
    authServer (Authenticated uClaim) = createCanvasHandler' uClaim :<|> getCanvasesHandler' uClaim
    authServer _ = throwAll err401 {errBody = "Unauthenticated"}

    createCanvasHandler' uClaim reqBody = do
      maybeUser <- liftIO $ getUserFromUserClaims pool uClaim
      case maybeUser of
        Just user -> createCanvasHandler user pool reqBody
        Nothing -> throwError err401 {errBody = "Unauthenticated"}

    getCanvasesHandler' uClaim = do
      maybeUser <- liftIO $ getUserFromUserClaims pool uClaim
      case maybeUser of
        Just user -> getCanvasesHandler user pool
        Nothing -> throwError err401 {errBody = "Unauthenticated"}
