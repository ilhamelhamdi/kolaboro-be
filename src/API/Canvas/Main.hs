{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Canvas.Main (CanvasAPI, canvasServer) where

import API.Canvas.CreateCanvas (CreateCanvasAPI, createCanvasHandler)
import API.Canvas.DeleteCanvas (DeleteCanvasAPI, deleteCanvasHandler)
import API.Canvas.GetCanvas (GetCanvasAPI, getCanvasHandler)
import API.Canvas.GetCanvases (GetCanvasesAPI, getCanvasesHandler)
import API.Canvas.UpdateCanvas (UpdateCanvasAPI, updateCanvasHandler)
import Control.Monad.IO.Class (liftIO)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Servant.Auth.Server
  ( Auth,
    AuthResult (Authenticated),
    JWT,
    ThrowAll (throwAll),
  )
import Utils.JWTUtils (UserClaims, getUserFromUserClaims)

type CanvasAPI =
  Auth '[JWT] UserClaims
    :> "canvas"
    :> ( CreateCanvasAPI
           :<|> GetCanvasesAPI
           :<|> GetCanvasAPI
           :<|> UpdateCanvasAPI
           :<|> DeleteCanvasAPI
       )

canvasServer :: Pool Connection -> Server CanvasAPI
canvasServer pool = authServer
  where
    authServer (Authenticated uClaim) = createCanvasHandler' uClaim :<|> getCanvasesHandler' uClaim :<|> getCanvasHandler' uClaim :<|> updateCanvasHandler' uClaim :<|> deleteCanvasHandler' uClaim
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

    getCanvasHandler' uClaim canvasId = do
      maybeUser <- liftIO $ getUserFromUserClaims pool uClaim
      case maybeUser of
        Just user -> getCanvasHandler user pool canvasId
        Nothing -> throwError err401 {errBody = "Unauthenticated"}

    updateCanvasHandler' uClaim canvasId reqBody = do
      maybeUser <- liftIO $ getUserFromUserClaims pool uClaim
      case maybeUser of
        Just user -> updateCanvasHandler user pool canvasId reqBody
        Nothing -> throwError err401 {errBody = "Unauthenticated"}

    deleteCanvasHandler' uClaim canvasId = do
      maybeUser <- liftIO $ getUserFromUserClaims pool uClaim
      case maybeUser of
        Just user -> deleteCanvasHandler user pool canvasId
        Nothing -> throwError err401 {errBody = "Unauthenticated"}