{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Note (Note(..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.List (intercalate)
import Data.Pool (withResource)
import Data.String (IsString (fromString))
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (FromRow, Only (..), execute, query)
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import Database.PostgreSQL.Simple.ToField (Action, ToField (toField))
import GHC.Generics (Generic)
import Repo.BaseRepo (BaseRepo (..), PGRepo (..), Predicate, toSqlWithParams)

data Note = Note
  { id :: Int,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    canvasId :: Int,
    authorId :: Int,
    subject :: String,
    body :: String,
    positionLeft :: Int,
    positionTop :: Int,
    width :: Int,
    zIndex :: Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON Note

instance FromJSON Note

instance FromRow Note where
  fromRow = Note <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

toRowWithoutId :: Note -> [Action]
toRowWithoutId Note {createdAt, updatedAt, canvasId, authorId, subject, body, positionLeft, positionTop, width, zIndex} =
  [ toField createdAt,
    toField updatedAt,
    toField canvasId,
    toField authorId,
    toField subject,
    toField body,
    toField positionLeft,
    toField positionTop,
    toField width,
    toField zIndex
  ]

noteFieldsWithoutId :: [String]
noteFieldsWithoutId = ["created_at", "updated_at", "canvas_id", "author_id", "subject", "body", "position_left", "position_top", "width", "z_index"]

instance BaseRepo (PGRepo Note) Note Int where
  findListByPredicate :: PGRepo Note -> Predicate -> IO [Note]
  findListByPredicate (PGRepo pool) predicate = do
    withResource pool $ \conn -> do
      let (whereClause, params) = toSqlWithParams predicate
      let q = "SELECT * FROM note WHERE " ++ whereClause
      query conn (fromString q) params

  create :: PGRepo Note -> Note -> IO Note
  create (PGRepo pool) note = do
    withResource pool $ \conn -> do
      let fieldNames = intercalate ", " noteFieldsWithoutId
      let questionMarks = intercalate ", " $ replicate (length noteFieldsWithoutId) "?"
      let q = "INSERT INTO note (" ++ fieldNames ++ ") VALUES (" ++ questionMarks ++ ") RETURNING *"
      [savedNote] <- query conn (fromString q) (toRowWithoutId note)
      return savedNote

  updateById :: PGRepo Note -> Int -> Note -> IO Note
  updateById (PGRepo pool) noteId note = do
    withResource pool $ \conn -> do
      let setFields = intercalate ", " (map (++ " = ?") noteFieldsWithoutId)
      let q = "UPDATE note SET " ++ setFields ++ " WHERE id = ? RETURNING *"
      [updatedNote] <- query conn (fromString q) (toRowWithoutId note ++ [toField noteId])
      return updatedNote

  deleteById :: PGRepo Note -> Int -> IO ()
  deleteById (PGRepo pool) noteId = do
    withResource pool $ \conn -> do
      let q = "DELETE FROM note WHERE id = ?"
      _ <- execute conn q (Only noteId)
      return ()