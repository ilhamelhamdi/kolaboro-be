{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module API.Note.CreateNote (CreateNoteAPI, createNoteHandler) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import DTO.NoteDTO (NoteDTO (..))
import DTO.StreamBaseDTO (StreamBaseDTO (..))
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Pool (Pool)
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple (Connection)
import Model.Note (Note (..))
import Model.User (User)
import qualified Model.User as User
import Repo.BaseRepo (BaseRepo (create), PGRepo (PGRepo))
import Servant
import Utils.ChannelUtils (TopicChannelMap, broadcastMessage)

type CreateNoteAPI = ReqBody '[JSON] NoteDTO :> Post '[JSON] ()

createNoteHandler :: Pool Connection -> TopicChannelMap -> User -> NoteDTO -> Handler ()
createNoteHandler dbPool channelMap user dto = do
  currentTime <- liftIO getCurrentTime

  let NoteDTO {canvasId, subject, body, positionLeft, positionTop, width, zIndex} = dto
  let note =
        Note
          { id = 0,
            createdAt = currentTime,
            updatedAt = currentTime,
            canvasId,
            authorId = User.id user,
            subject,
            body,
            positionLeft,
            positionTop,
            width,
            zIndex
          }
  savedNote <- liftIO $ create (PGRepo dbPool :: PGRepo Note) note

  let streamMessageDTO = StreamBaseDTO {event = "add_note", topic = canvasId, message = savedNote}
  let streamMessage = BL.unpack $ encode streamMessageDTO
  liftIO $ broadcastMessage channelMap canvasId streamMessage

  return ()
