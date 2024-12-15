{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Canvas.Main (CanvasAPI, canvasServer) where

import API.Canvas.CreateCanvas (CreateCanvasAPI, createCanvasHandler)
import API.Canvas.DeleteCanvas (DeleteCanvasAPI, deleteCanvasHandler)
import API.Canvas.GetCanvas (GetCanvasAPI, getCanvasHandler)
import API.Canvas.GetUserCanvases (GetCanvasesAPI, getCanvasesHandler)
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
import Utils.ChannelUtils (TopicChannelMap)
import Control.Concurrent.STM (TVar, readTVarIO)
import DTO.ResponseDTO (jsonError401)

type CanvasAPI =
  Auth '[JWT] UserClaims
    :> "canvas"
    :> ( CreateCanvasAPI
           :<|> GetCanvasesAPI
           :<|> GetCanvasAPI
           :<|> UpdateCanvasAPI
           :<|> DeleteCanvasAPI
       )

canvasServer :: Pool Connection -> TVar TopicChannelMap -> Server CanvasAPI
canvasServer pool channelMapVar = authServer
  where
    authServer (Authenticated uClaim) = createCanvasHandler' uClaim :<|> getCanvasesHandler' uClaim :<|> getCanvasHandler pool :<|> updateCanvasHandler' uClaim :<|> deleteCanvasHandler' uClaim
    authServer _ = throwAll $ jsonError401 "Unauthenticated" Nothing

    createCanvasHandler' uClaim reqBody = do
      maybeUser <- liftIO $ getUserFromUserClaims pool uClaim
      case maybeUser of
        Just user -> createCanvasHandler user pool reqBody
        Nothing -> throwError $ jsonError401 "Unauthenticated" Nothing

    getCanvasesHandler' uClaim = do
      maybeUser <- liftIO $ getUserFromUserClaims pool uClaim
      case maybeUser of
        Just user -> getCanvasesHandler user pool
        Nothing -> throwError $ jsonError401 "Unauthenticated" Nothing

    updateCanvasHandler' uClaim canvasId reqBody = do
      maybeUser <- liftIO $ getUserFromUserClaims pool uClaim
      channelMap <- liftIO $ readTVarIO channelMapVar
      case maybeUser of
        Just user -> updateCanvasHandler user pool channelMap canvasId reqBody
        Nothing -> throwError $ jsonError401 "Unauthenticated" Nothing

    deleteCanvasHandler' uClaim canvasId = do
      maybeUser <- liftIO $ getUserFromUserClaims pool uClaim
      case maybeUser of
        Just user -> deleteCanvasHandler user pool canvasId
        Nothing -> throwError $ jsonError401 "Unauthenticated" Nothing