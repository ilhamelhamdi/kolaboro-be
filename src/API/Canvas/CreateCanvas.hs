{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Canvas.CreateCanvas (CreateCanvasAPI, createCanvasHandler) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON)
import Data.Pool (Pool, withResource)
import Data.Time (UTCTime, getCurrentTime)
import Database.PostgreSQL.Simple (Connection, Only (Only), query)
import GHC.Generics (Generic)
import Model.Canvas (Canvas (..), CanvasTuple, Owner (..), userToOwner)
import qualified Model.Canvas as Canvas
import Model.User (User)
import qualified Model.User as User
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
  savedCanvas <- liftIO $ saveCanvas pool canvas
  case savedCanvas of
    Just c -> return c
    Nothing -> throwError err401 {errBody = "Failed to create canvas"}

saveCanvas :: Pool Connection -> Canvas -> IO (Maybe Canvas)
saveCanvas pool canvas = do
  liftIO $ withResource pool $ \conn -> do
    let q = "INSERT INTO canvas (title, namespace, address, owner_id, background, created_at, updated_at) VALUES (?, ?, ?, ?, ?, ?, ?) RETURNING *"
    canvases <- query conn q (toTupleWithoutId canvas) :: IO [CanvasTuple]
    case canvases of
      [] -> return Nothing
      _ -> do
        -- fetch owner first from user table
        let Canvas {owner = Owner {id = ownerId}} = canvas
        owner <- fetchOwner pool ownerId
        case owner of
          Nothing -> return Nothing
          Just o -> return $ Just $ (Canvas.fromTuple $ head canvases) {owner = o}
  where
    toTupleWithoutId :: Canvas -> (String, String, String, Int, String, UTCTime, UTCTime)
    toTupleWithoutId Canvas {title, namespace, address, owner, background, createdAt, updatedAt} =
      let Owner {id = ownerId} = owner
       in (title, namespace, address, ownerId, background, createdAt, updatedAt)

fetchOwner :: Pool Connection -> Int -> IO (Maybe Owner)
fetchOwner pool ownerId = do
  liftIO $ withResource pool $ \conn -> do
    let q = "SELECT * FROM users WHERE id=?"
    users <- query conn q (Only ownerId) :: IO [User.UserTuple]
    case users of
      [] -> return Nothing
      _ -> return $ Just $ userToOwner $ User.fromTuple $ head users