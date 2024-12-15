{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Profile.GetProfile (GetProfileAPI, getProfileHandler) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import DTO.ResponseDTO (ResponseDTO, jsonError404, successResponse)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Model.Profile (Profile)
import qualified Model.Profile as Profile
import Model.User (User)
import Repo.BaseRepo (BaseRepo (findByPredicate), PGRepo (PGRepo), Predicate (Or), equals)
import Servant

type GetProfileAPI = QueryParam "username" String :> QueryParam "id" Int :> Get '[JSON] (ResponseDTO Profile)

getProfileHandler :: Pool Connection -> Maybe String -> Maybe Int -> Handler (ResponseDTO Profile)
getProfileHandler pool username userId = do
  let predicate = Or ("username" `equals` username) ("id" `equals` userId)
  maybeUser <- liftIO $ findByPredicate (PGRepo pool :: PGRepo User) predicate
  case maybeUser of
    Nothing -> throwError $ jsonError404 "Failed to fetch user" (Just "User not found.")
    Just user -> do
      return $ successResponse "Profile fetched successfully" $ Just $ Profile.fromUser user