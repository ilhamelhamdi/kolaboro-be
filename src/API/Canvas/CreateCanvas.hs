{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module API.Canvas.CreateCanvas (CreateCanvasAPI, createCanvasHandler) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import DTO.CanvasDTO (CanvasRequestDTO (..))
import DTO.ResponseDTO (ResponseDTO, successResponse)
import Data.Pool (Pool)
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple (Connection)
import Model.Canvas (Canvas (..), userToOwner)
import qualified Model.Canvas as Canvas
import Model.User (User)
import Repo.BaseRepo (BaseRepo (create), PGRepo (PGRepo))
import Servant

type CreateCanvasAPI = ReqBody '[JSON] CanvasRequestDTO :> Post '[JSON] (ResponseDTO Canvas)

createCanvasHandler :: User -> Pool Connection -> CanvasRequestDTO -> Handler (ResponseDTO Canvas)
createCanvasHandler user pool reqBody = do
  currentTime <- liftIO getCurrentTime
  let CanvasRequestDTO
        { title = reqTitle,
          background = reqBackground
        } = reqBody
  let canvas =
        Canvas
          { id = 0,
            title = reqTitle,
            owner = userToOwner user,
            background = reqBackground,
            createdAt = currentTime,
            updatedAt = currentTime
          }
  savedCanvas <- liftIO $ create (PGRepo pool :: PGRepo Canvas) canvas
  let owner = Canvas.userToOwner user
  let res = savedCanvas {owner}
  return $ successResponse "Canvas created successfully" $ Just res
