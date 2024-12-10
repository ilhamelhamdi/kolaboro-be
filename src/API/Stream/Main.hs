{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Stream.Main (StreamApi, streamHandler) where

import Control.Concurrent (dupChan, newChan, readChan)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, readTVarIO, writeTVar)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (liftIO))
import DTO.StreamBaseDTO (StreamBaseDTO (..))
import Data.Aeson (encode)
import Data.ByteString.Char8 (ByteString, pack)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as Map
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Database.PostgreSQL.Simple as PG
import Model.Canvas (Canvas)
import Model.Note (Note )
import Model.User (User)
import Network.WebSockets (Connection, acceptRequest, rejectRequest, sendTextData, withPingThread)
-- import Network.WebSockets.PingPong (withPingPong)
import Repo.BaseRepo (BaseRepo (findById, findListByPredicate), PGRepo (PGRepo), equals)
import Servant
import Servant.API.WebSocket (WebSocketPending)
import Servant.Auth.Server (JWTSettings)
import Utils.ChannelUtils (TopicChannel, TopicChannelMap)
import Utils.JWTUtils (getUserFromUserClaims, verifiyToken)

type StreamApi = "stream" :> Capture "channelId" Int :> QueryParam "accessToken" Text :> WebSocketPending

streamHandler :: JWTSettings -> Pool PG.Connection -> TVar TopicChannelMap -> Server StreamApi
streamHandler jwtSetting dbPool channelMapVar channelId maybeToken penConn =
  liftIO $ do
    eitherUser <- authenticate
    eitherChannel <- getChannel
    setupConnection eitherUser eitherChannel
  where
    getChannel :: IO (Either Text TopicChannel)
    getChannel = do
      channelMap <- readTVarIO channelMapVar
      case Map.lookup channelId channelMap of
        Just chan -> return $ Right chan
        Nothing -> do
          maybeCanvas <- findById (PGRepo dbPool :: PGRepo Model.Canvas.Canvas) channelId
          case maybeCanvas of
            Nothing -> return $ Left "Channel not found"
            Just _ -> do
              chan <- newChan :: IO TopicChannel
              atomically $ writeTVar channelMapVar $ Map.insert channelId chan channelMap
              return $ Right chan

    authenticate :: IO (Either Text User)
    authenticate = case maybeToken of
      Nothing -> return $ Left "No token provided"
      Just token -> do
        result <- verifiyToken jwtSetting token
        case result of
          Nothing -> return $ Left "Invalid token"
          Just uClaim -> do
            maybeUser <- getUserFromUserClaims dbPool uClaim
            return $ maybe (Left "User not found") Right maybeUser

    setupConnection :: Either Text User -> Either Text TopicChannel -> IO ()
    setupConnection (Right _) (Right tChannel) = do
      conn <- acceptRequest penConn
      withPingThread conn 30 (return ()) $ do
        sendTextData conn ("Connected!" :: ByteString)
        refreshNotes conn
        rChannel <- dupChan tChannel
        listenChannel conn rChannel
    -- forever $ handleMessages conn tChannel user
    setupConnection (Left err) _ = rejectRequest penConn $ encodeUtf8 err
    setupConnection _ (Left err) = rejectRequest penConn $ encodeUtf8 err

    listenChannel :: Connection -> TopicChannel -> IO ()
    listenChannel conn rChannel = do
      forever $ do
        msg <- readChan rChannel
        sendTextData conn $ pack msg

    refreshNotes :: Connection -> IO ()
    refreshNotes conn = do
      let predicate = equals "canvas_id" channelId
      allNotes <- findListByPredicate (PGRepo dbPool :: PGRepo Model.Note.Note) predicate
      let streamDTO = StreamBaseDTO {event = "refresh_notes", topic = channelId, message = allNotes}
      sendTextData conn $ encode streamDTO
      return ()

-- handleMessages :: Connection -> TopicChannel -> User -> IO ()
-- handleMessages conn tChannel user = do
--   msg <- receiveData conn :: IO ByteString
--   case eitherDecode (BL.fromStrict msg) :: Either String StreamBaseDTO of
--     Left err -> sendTextData conn $ pack err
--     Right dto -> routeEventToHandler conn tChannel user dto

-- routeEventToHandler :: Connection -> TopicChannel -> User -> StreamBaseDTO -> IO ()
-- routeEventToHandler conn tChannel user dto = do
--   let handlers = Map.fromList [("add_note", handleEvent addNoteHandler)]
--   case Map.lookup (event dto) handlers of
--     Just handler -> handler conn tChannel user dto
--     Nothing -> sendTextData conn $ pack "Unknown event"

-- handleEvent :: (FromJSON a) => EventHandler a -> Connection -> TopicChannel -> User -> StreamBaseDTO -> IO ()
-- handleEvent action conn tChannel user dto =
--   case decodeMessage dto of
--     Left err -> sendTextData conn $ pack err
--     Right payload -> action conn dbPool tChannel user payload
--   where
--     decodeMessage :: (FromJSON a) => StreamBaseDTO -> Either String a
--     decodeMessage = eitherDecode . BL.fromStrict . pack . message
