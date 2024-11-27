{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

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

type GetCanvasAPI = Capture "canvasId" Int :> Get '[JSON] Canvas

getCanvasHandler :: User -> Pool Connection -> Int -> Handler Canvas
getCanvasHandler user pool canvasId = do
  let predicate = And (equals "owner_id" $ User.id user) (equals "id" canvasId)
  canvases <- liftIO $ findListByPredicate (PGRepo pool :: PGRepo Canvas) predicate
  let owner = Canvas.userToOwner user
  case canvases of
    [] -> throwError err401 {errBody = "Canvas not found"}
    canvas : _ -> return $ Canvas.setOwner canvas owner