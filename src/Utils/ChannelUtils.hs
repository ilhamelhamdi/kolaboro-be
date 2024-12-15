{-# LANGUAGE OverloadedStrings #-}

module Utils.ChannelUtils (TopicChannel, TopicChannelMap, broadcastMessage, getChannel) where

import Control.Concurrent
import Control.Concurrent.STM (TVar, atomically, readTVarIO, writeTVar)
import qualified Data.Map as Map
import Data.Pool (Pool)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import Model.Canvas (Canvas)
import Repo.BaseRepo (BaseRepo (findById), PGRepo (PGRepo))

-- Type for a canvas/topic connections map, where each canvas has its own channel
type TopicChannel = Chan String

type TopicChannelMap = Map.Map Int TopicChannel

-- Function to broadcast a message to all connections of a canvas/topic
broadcastMessage :: TopicChannelMap -> Int -> String -> IO ()
broadcastMessage topicMap topicId message = do
  case Map.lookup topicId topicMap of
    Just chan -> writeChan chan message
    Nothing -> do
      print $ "Channel not found for topic: " ++ show topicId
      return ()

getChannel :: Pool Connection -> TVar TopicChannelMap -> Int -> IO (Either Text TopicChannel)
getChannel dbPool channelMapVar channelId = do
  channelMap <- readTVarIO channelMapVar
  case Map.lookup channelId channelMap of
    Just chan -> return $ Right chan
    Nothing -> do
      maybeCanvas <- findById (PGRepo dbPool :: PGRepo Canvas) channelId
      case maybeCanvas of
        Nothing -> return $ Left "Channel not found"
        Just _ -> do
          chan <- newChan :: IO TopicChannel
          atomically $ writeTVar channelMapVar $ Map.insert channelId chan channelMap
          return $ Right chan
