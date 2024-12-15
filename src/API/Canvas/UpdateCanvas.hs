{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Canvas.UpdateCanvas where

import Control.Monad.IO.Class (MonadIO (liftIO))
import DTO.CanvasDTO (CanvasRequestDTO)
import qualified DTO.CanvasDTO as CanvasDTO
import Data.Pool (Pool)
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple (Connection)
import Model.Canvas (Canvas (..), findUserCanvasById)
import qualified Model.Canvas as Canvas
import Model.User (User)
import Repo.BaseRepo (BaseRepo (updateById), PGRepo (PGRepo))
import Servant
import DTO.ResponseDTO (jsonError404, ResponseDTO, successResponse)

type UpdateCanvasAPI = Capture "canvasId" Int :> ReqBody '[JSON] CanvasRequestDTO :> Put '[JSON] (ResponseDTO Canvas)

updateCanvasHandler :: User -> Pool Connection -> Int -> CanvasRequestDTO -> Handler (ResponseDTO Canvas)
updateCanvasHandler user pool canvasId canvasDTO = do
  let connPool = PGRepo pool :: PGRepo Canvas
  maybeOldCanvas <- liftIO $ findUserCanvasById connPool user canvasId
  case maybeOldCanvas of
    Nothing -> throwError $ jsonError404 "Failed to update canvas" (Just "Canvas not found.")
    Just oldCanvas -> do
      currentTime <- liftIO getCurrentTime
      let updatedCanvas =
            oldCanvas
              { title = CanvasDTO.title canvasDTO,
                namespace = CanvasDTO.namespace canvasDTO,
                address = CanvasDTO.address canvasDTO,
                background = CanvasDTO.background canvasDTO,
                updatedAt = currentTime
              }
      newCanvas <- liftIO $ updateById connPool canvasId updatedCanvas
      let res = newCanvas {owner = Canvas.userToOwner user}
      return $ successResponse "Canvas updated successfully" $ Just res