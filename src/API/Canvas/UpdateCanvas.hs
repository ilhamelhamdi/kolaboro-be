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

type UpdateCanvasAPI = Capture "canvasId" Int :> ReqBody '[JSON] CanvasRequestDTO :> Put '[JSON] Canvas

updateCanvasHandler :: User -> Pool Connection -> Int -> CanvasRequestDTO -> Handler Canvas
updateCanvasHandler user pool canvasId canvasDTO = do
  let connPool = PGRepo pool :: PGRepo Canvas
  maybeOldCanvas <- liftIO $ findUserCanvasById connPool user canvasId
  case maybeOldCanvas of
    Nothing -> throwError err404 {errBody = "Canvas not found"}
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
      return $ Canvas.setOwner newCanvas $ Canvas.userToOwner user
