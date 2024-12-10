{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Note.DeleteNote (DeleteNoteAPI, deleteNoteHandler) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import DTO.StreamBaseDTO (StreamBaseDTO (..))
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 as BL
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Model.Note (Note)
import qualified Model.Note as Note
import Model.User (User)
import qualified Model.User as User
import Repo.BaseRepo (BaseRepo (deleteById, findById), PGRepo (PGRepo))
import Servant
import Utils.ChannelUtils (TopicChannelMap, broadcastMessage)

type DeleteNoteAPI = Capture "noteId" Int :> Delete '[JSON] ()

deleteNoteHandler :: Pool Connection -> TopicChannelMap -> User -> Int -> Handler ()
deleteNoteHandler pool channelMap user noteId = do
  let connPool = PGRepo pool :: PGRepo Note
  maybeNote <- liftIO $ findById connPool noteId
  case maybeNote of
    Nothing -> throwError err404 {errBody = "Note not found"}
    Just note ->
      if Note.authorId note /= User.id user
        then throwError err403 {errBody = "Forbidden"}
        else do
          liftIO $ deleteById connPool noteId
          let streamDTO =
                StreamBaseDTO
                  { event = "delete_note",
                    topic = Note.canvasId note,
                    message = noteId
                  }
          let streamMessage = BL.unpack $ encode streamDTO
          liftIO $ broadcastMessage channelMap (Note.canvasId note) streamMessage