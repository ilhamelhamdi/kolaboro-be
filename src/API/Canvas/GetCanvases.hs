{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Canvas.GetCanvases (GetCanvasesAPI, getCanvasesHandler) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import DTO.ResponseDTO (ResponseDTO, successResponse)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Model.Canvas (Canvas)
import qualified Model.Canvas as Canvas
import Model.User (User)
import qualified Model.User as User
import Repo.BaseRepo (BaseRepo (findListByPredicate), PGRepo (PGRepo), equals)
import Servant
import Prelude hiding (id)

type GetCanvasesAPI = Get '[JSON] (ResponseDTO [Canvas])

getCanvasesHandler :: User -> Pool Connection -> Handler (ResponseDTO [Canvas])
getCanvasesHandler user pool = do
  canvases <- liftIO $ findCanvasesByOwnerId (User.id user) pool
  let owner = Canvas.userToOwner user
  let res = map (`Canvas.setOwner` owner) canvases
  return $ successResponse "Canvases fetched successfully" $ Just res
  where
    findCanvasesByOwnerId :: Int -> Pool Connection -> IO [Canvas]
    findCanvasesByOwnerId ownerId connPool = findListByPredicate (PGRepo connPool :: PGRepo Canvas) $ equals "owner_id" ownerId