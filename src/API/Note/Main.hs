{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Note.Main (NoteAPI, noteServer) where

import API.Note.CreateNote (CreateNoteAPI, createNoteHandler)
import API.Note.DeleteNote (DeleteNoteAPI, deleteNoteHandler)
import API.Note.UpdateNote (UpdateNoteAPI, updateNoteHandler)
import Control.Concurrent.STM (TVar, readTVarIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Servant.Auth (Auth, JWT)
import Servant.Auth.Server (AuthResult (Authenticated), ThrowAll (throwAll))
import Utils.ChannelUtils (TopicChannelMap)
import Utils.JWTUtils (UserClaims, getUserFromUserClaims)

type NoteAPI =
  Auth '[JWT] UserClaims
    :> "note"
    :> ( CreateNoteAPI
           :<|> DeleteNoteAPI
           :<|> UpdateNoteAPI
       )

noteServer :: Pool Connection -> TVar TopicChannelMap -> Server NoteAPI
noteServer pool channelMapVar = midServer
  where
    midServer (Authenticated uClaim) =
      createNoteHandler' uClaim
        :<|> deleteNoteHandler' uClaim
        :<|> updateNoteHandler' uClaim
    midServer _ = throwAll err401 {errBody = "Unauthenticated"}

    createNoteHandler' uClaim dto = do
      maybeUser <- liftIO $ getUserFromUserClaims pool uClaim
      channelMap <- liftIO $ readTVarIO channelMapVar
      case maybeUser of
        Just user -> createNoteHandler pool channelMap user dto
        Nothing -> throwError err401 {errBody = "Unauthenticated"}

    deleteNoteHandler' uClaim noteId = do
      maybeUser <- liftIO $ getUserFromUserClaims pool uClaim
      channelMap <- liftIO $ readTVarIO channelMapVar
      case maybeUser of
        Just user -> deleteNoteHandler pool channelMap user noteId
        Nothing -> throwError err401 {errBody = "Unauthenticated"}

    updateNoteHandler' uClaim noteId dto = do
      maybeUser <- liftIO $ getUserFromUserClaims pool uClaim
      channelMap <- liftIO $ readTVarIO channelMapVar
      case maybeUser of
        Just user -> updateNoteHandler pool channelMap user noteId dto
        Nothing -> throwError err401 {errBody = "Unauthenticated"}