{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Note.UpdateNote (updateNoteHandler, UpdateNoteAPI) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import DTO.NoteDTO (NoteDTO)
import qualified DTO.NoteDTO as NoteDTO
import DTO.ResponseDTO (ResponseDTO, jsonError403, jsonError404, successResponse)
import DTO.StreamBaseDTO (StreamBaseDTO (..))
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Pool (Pool)
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple (Connection)
import Model.Canvas (Canvas)
import qualified Model.Canvas as Canvas
import Model.Note (Note)
import qualified Model.Note as Note
import Model.User (User)
import qualified Model.User as User
import Repo.BaseRepo (BaseRepo (findById, updateById), PGRepo (PGRepo))
import Servant
import Utils.ChannelUtils (TopicChannelMap, broadcastMessage)

type UpdateNoteAPI = Capture "noteId" Int :> ReqBody '[JSON] NoteDTO :> Put '[JSON] (ResponseDTO Note)

updateNoteHandler :: Pool Connection -> TopicChannelMap -> User -> Int -> NoteDTO -> Handler (ResponseDTO Note)
updateNoteHandler pool channelMap user noteId noteDTO = do
  let connPool = PGRepo pool :: PGRepo Note
  maybeOldNote <- liftIO $ findById connPool noteId
  case maybeOldNote of
    Nothing -> throwError $ jsonError404 "Failed to update note" (Just "Note not found.")
    Just oldNote ->
      do
        canvas <- liftIO $ findById (PGRepo pool :: PGRepo Canvas) (Note.canvasId oldNote)
        case canvas of
          Nothing -> throwError $ jsonError404 "Failed to update note" (Just "Canvas not found.")
          Just canvasRecord ->
            do
              let Canvas.Owner {id = canvasAuhorId} = Canvas.owner canvasRecord
              if Note.authorId oldNote /= User.id user && canvasAuhorId /= User.id user
                then throwError $ jsonError403 "Forbidden" (Just "You are not the author of this note")
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
                  return $ successResponse "Note updated successfully" $ Just newNote