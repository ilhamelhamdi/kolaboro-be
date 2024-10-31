{-# LANGUAGE DeriveGeneric #-}

module Model.User (User (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time
import GHC.Generics (Generic)
import Servant.Auth.JWT

data User = User
  { email :: String,
    password :: String,
    name :: String,
    registered_date :: UTCTime
  }
  deriving (Eq, Show, Generic)

instance ToJSON User

instance FromJSON User

instance ToJWT User

instance FromJWT User