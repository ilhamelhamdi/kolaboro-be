{-# LANGUAGE DeriveGeneric #-}

module DTO.NoteDTO (NoteDTO (..)) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data NoteDTO = NoteDTO
  { canvasId :: Int,
    subject :: String,
    body :: String,
    positionLeft :: Int,
    positionTop :: Int,
    width :: Int,
    zIndex :: Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON NoteDTO

instance FromJSON NoteDTO