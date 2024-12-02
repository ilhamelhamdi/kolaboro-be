{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Canvas.DeleteCanvas (DeleteCanvasAPI, deleteCanvasHandler) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Model.Canvas (Canvas, findUserCanvasById)
import Model.User (User)
import Repo.BaseRepo (BaseRepo (deleteById), PGRepo (PGRepo))
import Servant

type DeleteCanvasAPI = Capture "canvasId" Int :> Delete '[JSON] ()

deleteCanvasHandler :: User -> Pool Connection -> Int -> Handler ()
deleteCanvasHandler user pool canvasId = do
  let connPool = PGRepo pool :: PGRepo Canvas
  canvas <- liftIO $ findUserCanvasById connPool user canvasId
  case canvas of
    Nothing -> throwError err404 {errBody = "Canvas not found"}
    Just _ -> liftIO $ deleteById connPool canvasId