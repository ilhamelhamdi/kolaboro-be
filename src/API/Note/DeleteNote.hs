{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Note.DeleteNote (DeleteNoteAPI, deleteNoteHandler) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import DTO.ResponseDTO (ResponseDTO, jsonError403, jsonError404, successResponse)
import DTO.StreamBaseDTO (StreamBaseDTO (..))
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 as BL
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Model.Canvas (Canvas)
import qualified Model.Canvas as Canvas
import Model.Note (Note)
import qualified Model.Note as Note
import Model.User (User)
import qualified Model.User as User
import Repo.BaseRepo (BaseRepo (deleteById, findById), PGRepo (PGRepo))
import Servant
import Utils.ChannelUtils (TopicChannelMap, broadcastMessage)

type DeleteNoteAPI = Capture "noteId" Int :> Delete '[JSON] (ResponseDTO String)

deleteNoteHandler :: Pool Connection -> TopicChannelMap -> User -> Int -> Handler (ResponseDTO String)
deleteNoteHandler pool channelMap user noteId = do
  let connPool = PGRepo pool :: PGRepo Note
  maybeNote <- liftIO $ findById connPool noteId
  case maybeNote of
    Nothing -> throwError $ jsonError404 "Failed to delete note" (Just "Note not found.")
    Just note -> do
      canvas <- liftIO $ findById (PGRepo pool :: PGRepo Canvas) (Note.canvasId note)
      case canvas of
        Nothing -> throwError $ jsonError404 "Failed to delete note" (Just "Canvas not found.")
        Just canvasRecord ->
          do
            let Canvas.Owner {id = canvasAuhorId} = Canvas.owner canvasRecord
            if Note.authorId note /= User.id user && canvasAuhorId /= User.id user
              then throwError $ jsonError403 "Forbidden" (Just "You are not authorized to delete this note.")
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
                return $ successResponse "Note deleted successfully" Nothing