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

getCanvasHandler :: User -> Pool Connection -> Int -> Handler (ResponseDTO Canvas)
getCanvasHandler user pool canvasId = do
  maybeCanvas <- liftIO $ findById (PGRepo pool :: PGRepo Canvas) canvasId
  let owner = Canvas.userToOwner user
  case maybeCanvas of
    Nothing -> throwError $ jsonError404 "Failed to fetch canvas" (Just "Canvas not found.")
    Just canvas -> do
      let res = canvas {Canvas.owner}
      return $ successResponse "Canvas fetched successfully" $ Just res