module Utils.ChannelUtils (TopicChannel, TopicChannelMap, broadcastMessage) where

import Control.Concurrent
import qualified Data.Map as Map

-- Type for a canvas/topic connections map, where each canvas has its own channel
type TopicChannel = Chan String

type TopicChannelMap = Map.Map Int TopicChannel

-- Function to broadcast a message to all connections of a canvas/topic
broadcastMessage :: TopicChannelMap -> Int -> String -> IO ()
broadcastMessage topicMap topicId message = do
  case Map.lookup topicId topicMap of
    Just chan -> writeChan chan message
    Nothing -> return ()
