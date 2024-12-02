{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Model.Canvas (Canvas (..), Owner (..), userToOwner, setOwner, findUserCanvasById) where

import Data.Aeson (FromJSON, ToJSON)
import Data.List (intercalate)
import Data.Pool (withResource)
import Data.String (fromString)
import Data.Time
import Database.PostgreSQL.Simple (FromRow, Only (..), execute, query)
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import Database.PostgreSQL.Simple.ToField (Action, ToField (toField))
import GHC.Generics (Generic)
import Model.User (User)
import qualified Model.User as User
import Repo.BaseRepo (BaseRepo (..), PGRepo (..), Predicate, toSqlWithParams)
import Prelude hiding (id)

data Canvas = Canvas
  { id :: Int,
    title :: String,
    namespace :: String,
    address :: String,
    owner :: Owner,
    background :: String,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Eq, Show, Generic)

instance ToJSON Canvas

instance FromJSON Canvas

instance FromRow Canvas where
  fromRow = Canvas <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

toRowWithoutId :: Canvas -> [Action]
toRowWithoutId Canvas {title, namespace, address, owner, background, createdAt, updatedAt} =
  [ toField title,
    toField namespace,
    toField address,
    toField owner,
    toField background,
    toField createdAt,
    toField updatedAt
  ]

canvasFieldsWithoutId :: [String]
canvasFieldsWithoutId = ["title", "namespace", "address", "owner_id", "background", "created_at", "updated_at"]

instance BaseRepo (PGRepo Canvas) Canvas Int where
  findListByPredicate :: PGRepo Canvas -> Predicate -> IO [Canvas]
  findListByPredicate (PGRepo pool) predicate = do
    withResource pool $ \conn -> do
      let (whereClause, params) = toSqlWithParams predicate
      let q = "SELECT * FROM canvas WHERE " ++ whereClause
      query conn (fromString q) params

  create :: PGRepo Canvas -> Canvas -> IO Canvas
  create (PGRepo pool) canvas = do
    withResource pool $ \conn -> do
      let fieldNames = intercalate ", " canvasFieldsWithoutId
      let questionMarks = intercalate ", " $ replicate (length canvasFieldsWithoutId) "?"
      let q = "INSERT INTO canvas (" ++ fieldNames ++ ") VALUES (" ++ questionMarks ++ ") RETURNING *"
      [savedCanvas] <- query conn (fromString q) (toRowWithoutId canvas)
      return savedCanvas

  updateById :: PGRepo Canvas -> Int -> Canvas -> IO Canvas
  updateById (PGRepo pool) canvasId canvas = do
    withResource pool $ \conn -> do
      let setFields = intercalate ", " (map (++ " = ?") canvasFieldsWithoutId)
      let q = "UPDATE canvas SET " ++ setFields ++ " WHERE id = ? RETURNING *"
      [updatedCanvas] <- query conn (fromString q) (toRowWithoutId canvas ++ [toField canvasId])
      return updatedCanvas

  deleteById :: PGRepo Canvas -> Int -> IO ()
  deleteById (PGRepo pool) canvasId = do
    withResource pool $ \conn -> do
      let q = "DELETE FROM canvas WHERE id = ?"
      _ <- execute conn q (Only canvasId)
      return ()

findUserCanvasById :: PGRepo Canvas -> User -> Int -> IO (Maybe Canvas)
findUserCanvasById (PGRepo pool) user canvasId = do
  withResource pool $ \conn -> do
    let q = "SELECT * FROM canvas WHERE id = ? AND owner_id = ?"
    maybeCanvas <- query conn q (canvasId, User.id user)
    case maybeCanvas of
      [] -> return Nothing
      canvas : _ -> return $ Just canvas

setOwner :: Canvas -> Owner -> Canvas
setOwner canvas owner = canvas {owner = owner}

data Owner = Owner
  { id :: Int,
    username :: String,
    displayName :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Owner

instance FromJSON Owner

instance FromField Owner where
  fromField field_ mdata = do
    ownerId <- fromField field_ mdata
    return Owner {id = ownerId, username = "", displayName = ""}

instance ToField Owner where
  toField Owner {id = ownerId} = toField ownerId

userToOwner :: User -> Owner
userToOwner User.User {id, username, display_name} = Owner {id, username, displayName = display_name}