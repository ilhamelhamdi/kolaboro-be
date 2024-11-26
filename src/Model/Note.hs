{-# LANGUAGE DeriveGeneric #-}

module Model.Note (Note, Position) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data Note = Note
  { id :: Int,
    canvasId :: Int,
    authorId :: Int,
    subject :: String,
    body :: String,
    position :: Position,
    width :: Int,
    zIndex :: Int,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Eq, Show, Generic)

instance ToJSON Note

instance FromJSON Note

data Position = Position
  { left :: Int,
    right :: Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON Position

instance FromJSON Position
