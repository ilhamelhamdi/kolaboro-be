{-# LANGUAGE DeriveGeneric #-}

module DTO.StreamBaseDTO (StreamBaseDTO (..)) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data StreamBaseDTO a = StreamBaseDTO
  { event :: String,
    topic :: Int,
    message :: a
  }
  deriving (Eq, Show, Generic)

instance (ToJSON a, FromJSON a) => ToJSON (StreamBaseDTO a)
instance (ToJSON a, FromJSON a) => FromJSON (StreamBaseDTO a)