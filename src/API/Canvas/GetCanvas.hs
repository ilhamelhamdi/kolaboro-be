{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Canvas.GetCanvas (GetCanvasAPI, getCanvasHandler) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import DTO.ResponseDTO (ResponseDTO, jsonError404, successResponse)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Model.Canvas (Canvas)
import qualified Model.Canvas as Canvas
import Model.User (User)
import Repo.BaseRepo (BaseRepo (findById), PGRepo (..))
import Servant
import Prelude hiding (id)

type GetCanvasAPI = Capture "canvasId" Int :> Get '[JSON] (ResponseDTO Canvas)

getCanvasHandler :: Pool Connection -> Int -> Handler (ResponseDTO Canvas)
getCanvasHandler pool canvasId = do
  maybeCanvas <- liftIO $ findById (PGRepo pool :: PGRepo Canvas) canvasId
  case maybeCanvas of
    Nothing -> throwError $ jsonError404 "Failed to fetch canvas" (Just "Canvas not found.")
    Just canvas -> do
      let Canvas.Owner {id = ownerId} = Canvas.owner canvas
      maybeUser <- liftIO $ findById (PGRepo pool :: PGRepo User) ownerId
      case maybeUser of
        Nothing -> throwError $ jsonError404 "Failed to fetch canvas" (Just "Canvas owner not found.")
        Just user -> do
          let res = canvas {Canvas.owner = Canvas.userToOwner user}
          return $ successResponse "Canvas fetched successfully" $ Just res