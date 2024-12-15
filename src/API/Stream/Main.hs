{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Stream.Main (StreamApi, streamHandler) where

import Control.Concurrent (dupChan, readChan)
import Control.Concurrent.STM.TVar (TVar)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (liftIO))
import DTO.StreamBaseDTO (StreamBaseDTO (..))
import Data.Aeson (encode)
import Data.ByteString.Char8 (pack)
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Database.PostgreSQL.Simple as PG
import Model.Note (Note)
import Model.User (User)
import Network.WebSockets (Connection, acceptRequest, rejectRequest, sendTextData, withPingThread)
-- import Network.WebSockets.PingPong (withPingPong)
import Repo.BaseRepo (BaseRepo (findListByPredicate), PGRepo (PGRepo), equals)
import Servant
import Servant.API.WebSocket (WebSocketPending)
import Servant.Auth.Server (JWTSettings)
import Utils.ChannelUtils (TopicChannel, TopicChannelMap, getChannel)
import Utils.JWTUtils (getUserFromUserClaims, verifiyToken)

type StreamApi = "stream" :> Capture "channelId" Int :> QueryParam "accessToken" Text :> WebSocketPending

streamHandler :: JWTSettings -> Pool PG.Connection -> TVar TopicChannelMap -> Server StreamApi
streamHandler jwtSetting dbPool channelMapVar channelId maybeToken penConn =
  liftIO $ do
    eitherUser <- authenticate
    eitherChannel <- getChannel dbPool channelMapVar channelId
    setupConnection eitherUser eitherChannel
  where
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
      withPingThread conn 5 (return ()) $ do
        refreshNotes conn
        rChannel <- dupChan tChannel
        print "Listening to channel"
        listenChannel conn rChannel
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
      allNotes <- findListByPredicate (PGRepo dbPool :: PGRepo Note) predicate
      let streamDTO = StreamBaseDTO {event = "refresh_notes", topic = channelId, message = allNotes}
      sendTextData conn $ encode streamDTO
      return ()
