{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Model.Profile (Profile (..), fromUser) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Model.User (User (..))
import Prelude hiding (id)

data Profile = Profile
  { id :: Int,
    username :: String,
    display_name :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Profile

fromUser :: User -> Profile
fromUser User {..} = Profile {id, username, display_name}