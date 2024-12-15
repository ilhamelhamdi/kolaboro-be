{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Canvas.UpdateCanvas where

import Control.Monad.IO.Class (MonadIO (liftIO))
import DTO.CanvasDTO (CanvasRequestDTO)
import qualified DTO.CanvasDTO as CanvasDTO
import DTO.ResponseDTO (ResponseDTO, jsonError404, successResponse)
import DTO.StreamBaseDTO (StreamBaseDTO (..))
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Pool (Pool)
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple (Connection)
import Model.Canvas (Canvas (..), findUserCanvasById)
import qualified Model.Canvas as Canvas
import Model.User (User)
import Repo.BaseRepo (BaseRepo (updateById), PGRepo (PGRepo))
import Servant
import Utils.ChannelUtils (TopicChannelMap, broadcastMessage)

type UpdateCanvasAPI = Capture "canvasId" Int :> ReqBody '[JSON] CanvasRequestDTO :> Put '[JSON] (ResponseDTO Canvas)

updateCanvasHandler :: User -> Pool Connection -> TopicChannelMap -> Int -> CanvasRequestDTO -> Handler (ResponseDTO Canvas)
updateCanvasHandler user pool channelMap canvasId canvasDTO = do
  let connPool = PGRepo pool :: PGRepo Canvas
  maybeOldCanvas <- liftIO $ findUserCanvasById connPool user canvasId
  case maybeOldCanvas of
    Nothing -> throwError $ jsonError404 "Failed to update canvas" (Just "Canvas not found.")
    Just oldCanvas -> do
      currentTime <- liftIO getCurrentTime
      let updatedCanvas =
            oldCanvas
              { title = CanvasDTO.title canvasDTO,
                background = CanvasDTO.background canvasDTO,
                updatedAt = currentTime
              }
      newCanvas <- liftIO $ updateById connPool canvasId updatedCanvas
      let res = newCanvas {owner = Canvas.userToOwner user}
      let streamDTO =
            StreamBaseDTO
              { event = "edit_canvas",
                topic = canvasId,
                message = res
              }
      let streamMessage = BL.unpack $ encode streamDTO
      liftIO $ broadcastMessage channelMap canvasId streamMessage
      return $ successResponse "Canvas updated successfully" $ Just res