{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Canvas.GetCanvas (GetCanvasAPI, getCanvasHandler) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection, query)
import Model.Canvas (Canvas, CanvasTuple)
import qualified Model.Canvas as Canvas
import Model.User (User)
import qualified Model.User as User
import Servant
import Prelude hiding (id)

type GetCanvasAPI = Capture "productid" Int :> Get '[JSON] [Canvas]

getCanvasHandler :: User -> Pool Connection -> Int -> Handler Canvas
getCanvasHandler user pool canvasId = do
  canvases <- liftIO $ withResource pool $ \conn -> do
    let q = "SELECT * FROM canvas WHERE owner_id=? AND id=?"
    query conn q (User.id user, canvasId) :: IO [CanvasTuple]
  case canvases of
    [] -> throwError err401 {errBody = "Canvas not found"}
    _ -> return $ Canvas.fromTuple $ head canvases