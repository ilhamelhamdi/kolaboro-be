{-# LANGUAGE DeriveGeneric #-}

module Model.User (User(..)) where

import Data.Aeson
import Data.Time
import GHC.Generics (Generic)

data User = User
  { username :: String,
    password :: String,
    email :: String,
    name :: String,
    registered_date :: UTCTime
  }
  deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User