{-# LANGUAGE DeriveGeneric #-}

module DTO.CanvasDTO (CanvasRequestDTO (..)) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data CanvasRequestDTO = CanvasRequestDTO
  { title :: String,
    namespace :: String,
    address :: String,
    background :: String
  }
  deriving (Eq, Show, Generic)

instance FromJSON CanvasRequestDTO

instance ToJSON CanvasRequestDTO