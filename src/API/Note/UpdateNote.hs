{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Note.UpdateNote (updateNoteHandler, UpdateNoteAPI) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import DTO.NoteDTO (NoteDTO)
import qualified DTO.NoteDTO as NoteDTO
import DTO.StreamBaseDTO (StreamBaseDTO (..))
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Pool (Pool)
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple (Connection)
import Model.Note (Note)
import qualified Model.Note as Note
import Model.User (User)
import qualified Model.User as User
import Repo.BaseRepo (BaseRepo (findById, updateById), PGRepo (PGRepo))
import Servant
import Utils.ChannelUtils (TopicChannelMap, broadcastMessage)

type UpdateNoteAPI = Capture "noteId" Int :> ReqBody '[JSON] NoteDTO :> Put '[JSON] Note

updateNoteHandler :: Pool Connection -> TopicChannelMap -> User -> Int -> NoteDTO -> Handler Note
updateNoteHandler pool channelMap user noteId noteDTO = do
  let connPool = PGRepo pool :: PGRepo Note
  maybeOldNote <- liftIO $ findById connPool noteId
  case maybeOldNote of
    Nothing -> throwError err404 {errBody = "Note not found"}
    Just oldNote ->
      if Note.authorId oldNote /= User.id user
        then throwError err403 {errBody = "Forbidden"}
        else do
          currentTime <- liftIO getCurrentTime
          let updatedNote =
                oldNote
                  { Note.canvasId = NoteDTO.canvasId noteDTO,
                    Note.subject = NoteDTO.subject noteDTO,
                    Note.body = NoteDTO.body noteDTO,
                    Note.positionLeft = NoteDTO.positionLeft noteDTO,
                    Note.positionTop = NoteDTO.positionTop noteDTO,
                    Note.width = NoteDTO.width noteDTO,
                    Note.zIndex = NoteDTO.zIndex noteDTO,
                    Note.updatedAt = currentTime
                  }
          newNote <- liftIO $ updateById connPool noteId updatedNote
          let streamDTO =
                StreamBaseDTO
                  { event = "edit_note",
                    topic = Note.canvasId newNote,
                    message = newNote
                  }
          let streamMessage = BL.unpack $ encode streamDTO
          liftIO $ broadcastMessage channelMap (Note.canvasId newNote) streamMessage
          return newNote