module API.Stream.EventHandler (EventHandler) where

import Data.Pool (Pool)
import qualified Database.PostgreSQL.Simple as DB
import Model.User (User)
import qualified Network.WebSockets as WS
import Utils.ChannelUtils (TopicChannel)

type EventHandler a =
  WS.Connection ->
  Pool DB.Connection ->
  TopicChannel ->
  User ->
  a ->
  IO ()