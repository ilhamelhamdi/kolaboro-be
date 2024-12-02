{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Model.User (User (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.List (intercalate)
import Data.Pool (withResource)
import Data.String (IsString (fromString))
import Data.Time
import Database.PostgreSQL.Simple (FromRow, Only (..), execute, query)
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import Database.PostgreSQL.Simple.ToField (Action, ToField (toField))
import GHC.Generics (Generic)
import Repo.BaseRepo (BaseRepo (create, deleteById, findListByPredicate, updateById), PGRepo (..), Predicate, toSqlWithParams)
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

instance FromJSON User

instance ToJSON User

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field <*> field

toRowWithoutId :: User -> [Action]
toRowWithoutId User {email, password, username, display_name, registered_at, modified_at} =
  [ toField email,
    toField password,
    toField username,
    toField display_name,
    toField registered_at,
    toField modified_at
  ]

userFieldsWithoutId :: [String]
userFieldsWithoutId = ["email", "password", "username", "display_name", "registered_at", "modified_at"]

instance BaseRepo (PGRepo User) User Int where
  findListByPredicate :: PGRepo User -> Predicate -> IO [User]
  findListByPredicate (PGRepo pool) predicate = withResource pool $ \conn -> do
    let (whereClause, params) = toSqlWithParams predicate
    let q = "SELECT * FROM users WHERE " ++ whereClause
    query conn (fromString q) params :: IO [User]

  create :: PGRepo User -> User -> IO User
  create (PGRepo pool) user = withResource pool $ \conn -> do
    let fieldNames = intercalate ", " userFieldsWithoutId
    let questionMarks = intercalate ", " $ replicate (length userFieldsWithoutId) "?"
    let q = "INSERT INTO users (" ++ fieldNames ++ ") VALUES (" ++ questionMarks ++ ") RETURNING *"
    [savedUser] <- query conn (fromString q) (toRowWithoutId user)
    return savedUser

  updateById :: PGRepo User -> Int -> User -> IO User
  updateById (PGRepo pool) userId user = withResource pool $ \conn -> do
    let setFields = intercalate ", " (map (++ " = ?") userFieldsWithoutId)
    let q = "UPDATE users SET " ++ setFields ++ " WHERE id = ? RETURNING *"
    [updatedUser] <- query conn (fromString q) (toRowWithoutId user ++ [toField userId])
    return updatedUser

  deleteById :: PGRepo User -> Int -> IO ()
  deleteById (PGRepo pool) userId = withResource pool $ \conn -> do
    let q = "DELETE FROM users WHERE id = ?"
    _ <- execute conn q (Only userId)
    return ()