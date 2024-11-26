{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Canvas.GetCanvases (GetCanvasesAPI, getCanvasesHandler) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection, Only (Only), query)
import Model.Canvas (Canvas, CanvasTuple)
import qualified Model.Canvas as Canvas
import Model.User (User)
import qualified Model.User as User
import Servant
import Prelude hiding (id)

type GetCanvasesAPI = Get '[JSON] [Canvas]

getCanvasesHandler :: User -> Pool Connection -> Handler [Canvas]
getCanvasesHandler user pool = liftIO $ withResource pool $ \conn -> do
  let q = "SELECT * FROM canvas WHERE owner_id=?"
  canvases <- query conn q (Only $ User.id user) :: IO [CanvasTuple]
  let owner =  Canvas.userToOwner user
  return $ map Canvas.fromTuple canvases
