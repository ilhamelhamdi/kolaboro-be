{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Model.User (User (..), UserTuple, toTuple, fromTuple) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time
import GHC.Generics (Generic)
import Servant.Auth.JWT
import Prelude hiding (id)

data User = User
  { id :: Int,
    email :: String,
    password :: String,
    username :: String,
    display_name :: String,
    registered_at :: UTCTime,
    modified_at :: UTCTime
  }
  deriving (Eq, Show, Generic)

instance ToJSON User

instance FromJSON User

instance ToJWT User

type UserTuple = (Int, String, String, String, String, UTCTime, UTCTime)

toTuple :: User -> UserTuple
toTuple user = (user.id, email user, password user, username user, display_name user, registered_at user, modified_at user)

fromTuple :: UserTuple -> User
fromTuple (userId, userEmail, userPassword, userName, userDisplayName, registeredAt, modifiedAt) =
  User
    { id = userId,
      email = userEmail,
      password = userPassword,
      username = userName,
      display_name = userDisplayName,
      registered_at = registeredAt,
      modified_at = modifiedAt
    }