{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

-- Minimal Pragma

module Repo.BaseRepo (BaseRepo (..), PGRepo (..), Predicate (..), toSqlWithParams, equals, greaterThan, lessThan) where

import Data.Maybe (listToMaybe)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.ToField (Action, ToField (toField))

class (ToField id) => BaseRepo repo entity id | repo -> entity, entity -> id where
  findListByPredicate :: repo -> Predicate -> IO [entity]
  create :: repo -> entity -> IO entity
  updateById :: repo -> id -> entity -> IO entity
  deleteById :: repo -> id -> IO ()

  findByPredicate :: repo -> Predicate -> IO (Maybe entity)
  findByPredicate repo predicate = do
    results <- findListByPredicate repo predicate
    return $ listToMaybe results

  findById :: repo -> id -> IO (Maybe entity)
  findById repo entityId = do
    results <- findListByPredicate repo (equals "id" entityId)
    return $ listToMaybe results

  findAll :: repo -> IO [entity]
  findAll repo = findListByPredicate repo TruePredicate
  {-# MINIMAL create, updateById, deleteById, findListByPredicate #-}

newtype PGRepo entity = PGRepo (Pool Connection)

data Predicate
  = TruePredicate
  | Equals String Action
  | GreaterThan String Action
  | LessThan String Action
  | And Predicate Predicate
  | Or Predicate Predicate
  deriving (Show)

toSqlWithParams :: Predicate -> (String, [Action])
toSqlWithParams TruePredicate = ("1 = ?", [toField (1 :: Int)])
toSqlWithParams (Equals field value) = (field ++ " = ?", [value])
toSqlWithParams (GreaterThan field value) = (field ++ " > ?", [value])
toSqlWithParams (LessThan field value) = (field ++ " < ?", [value])
toSqlWithParams (And p1 p2) =
  let (sql1, params1) = toSqlWithParams p1
      (sql2, params2) = toSqlWithParams p2
   in ("(" ++ sql1 ++ " AND " ++ sql2 ++ ")", params1 ++ params2)
toSqlWithParams (Or p1 p2) =
  let (sql1, params1) = toSqlWithParams p1
      (sql2, params2) = toSqlWithParams p2
   in ("(" ++ sql1 ++ " OR " ++ sql2 ++ ")", params1 ++ params2)

equals :: (ToField a) => String -> a -> Predicate
equals field value = Equals field (toField value)

greaterThan :: (ToField a) => String -> a -> Predicate
greaterThan field value = GreaterThan field (toField value)

lessThan :: (ToField a) => String -> a -> Predicate
lessThan field value = LessThan field (toField value)
