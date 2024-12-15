{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NamedFieldPuns #-}

module API.Canvas.GetCanvas (GetCanvasAPI, getCanvasHandler) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Model.Canvas (Canvas)
import qualified Model.Canvas as Canvas
import Model.User (User)
import qualified Model.User as User
import Repo.BaseRepo (BaseRepo (findListByPredicate), PGRepo (..), Predicate (And), equals)
import Servant
import Prelude hiding (id)
import DTO.ResponseDTO (jsonError404, ResponseDTO, successResponse)

type GetCanvasAPI = Capture "canvasId" Int :> Get '[JSON] (ResponseDTO Canvas)

getCanvasHandler :: User -> Pool Connection -> Int -> Handler (ResponseDTO Canvas)
getCanvasHandler user pool canvasId = do
  let predicate = And (equals "owner_id" $ User.id user) (equals "id" canvasId)
  canvases <- liftIO $ findListByPredicate (PGRepo pool :: PGRepo Canvas) predicate
  let owner = Canvas.userToOwner user
  case canvases of
    [] -> throwError $ jsonError404 "Failed to fetch canvas" (Just "Canvas not found.")
    canvas : _ -> do
        let res = canvas {Canvas.owner}
        return $ successResponse "Canvas fetched successfully" $ Just res