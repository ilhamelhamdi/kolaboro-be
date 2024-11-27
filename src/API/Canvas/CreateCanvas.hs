{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Canvas.CreateCanvas (CreateCanvasAPI, createCanvasHandler) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON)
import Data.Pool (Pool)
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import Model.Canvas (Canvas (..), userToOwner)
import qualified Model.Canvas as Canvas
import Model.User (User)
import Repo.BaseRepo (BaseRepo (create), PGRepo (PGRepo))
import Servant

data CreateCanvasRequest = CreateCanvasRequest
  { title :: String,
    namespace :: String,
    address :: String,
    background :: String
  }
  deriving (Eq, Show, Generic)

instance FromJSON CreateCanvasRequest

instance ToJSON CreateCanvasRequest

type CreateCanvasAPI = ReqBody '[JSON] CreateCanvasRequest :> Post '[JSON] Canvas

createCanvasHandler :: User -> Pool Connection -> CreateCanvasRequest -> Handler Canvas
createCanvasHandler user pool reqBody = do
  currentTime <- liftIO getCurrentTime
  let CreateCanvasRequest
        { title = reqTitle,
          namespace = reqNamespace,
          address = reqAddress,
          background = reqBackground
        } = reqBody
  let canvas =
        Canvas
          { id = 0,
            title = reqTitle,
            namespace = reqNamespace,
            address = reqAddress,
            owner = userToOwner user,
            background = reqBackground,
            createdAt = currentTime,
            updatedAt = currentTime
          }
  savedCanvas <- liftIO $ create (PGRepo pool :: PGRepo Canvas) canvas
  let owner = Canvas.userToOwner user
  return savedCanvas {owner}
