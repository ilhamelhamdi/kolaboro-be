{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Model.Canvas (Canvas (..), Owner (..), userToOwner, CanvasTuple, toTuple, fromTuple) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time
import GHC.Generics (Generic (from))
import Model.User (User (..))
import Prelude hiding (id)

data Canvas = Canvas
  { id :: Int,
    title :: String,
    namespace :: String,
    address :: String,
    owner :: Owner,
    background :: String,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Eq, Show, Generic)

instance ToJSON Canvas

instance FromJSON Canvas

type CanvasTuple = (Int, String, String, String, Int, String, UTCTime, UTCTime)

toTuple :: Canvas -> CanvasTuple
toTuple Canvas {id, title, namespace, address, owner, background, createdAt, updatedAt} =
  let Owner {id = ownerId} = owner
   in (id, title, namespace, address, ownerId, background, createdAt, updatedAt)

fromTuple :: CanvasTuple -> Canvas
fromTuple (id, title, namespace, address, ownerId, background, createdAt, updatedAt) =
  Canvas
    { id,
      title,
      namespace,
      address,
      owner = Owner {id = ownerId, username = "", displayName = ""},
      background,
      createdAt,
      updatedAt
    }



data Owner = Owner
  { id :: Int,
    username :: String,
    displayName :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Owner

instance FromJSON Owner

userToOwner :: User -> Owner
userToOwner User {id, username, display_name} = Owner {id, username, displayName = display_name}