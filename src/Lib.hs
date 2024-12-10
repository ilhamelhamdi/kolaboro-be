{-# LANGUAGE DataKinds #-}

module Lib (startApp) where

import API.Auth.Main (authServer)
import API.Canvas.Main (canvasServer)
import API.Note.Main (noteServer)
import API.Protected (protectedHandler)
import API.Root (API)
import API.Stream.Main (streamHandler)
import API.User (userServer)
import DB.DBManager
import qualified Data.Map as Map
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import GHC.Conc (TVar, newTVarIO)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Auth.Server (JWTSettings, defaultCookieSettings)
import Utils.ChannelUtils (TopicChannelMap)
import Utils.JWTUtils (initJWTSettings)
import Network.Wai.Middleware.Cors (simpleCors)

api :: Proxy API
api = Proxy

server :: JWTSettings -> Pool Connection -> TVar TopicChannelMap -> Server API
server jwtSettings pool channelMap =
  userServer
    :<|> authServer jwtSettings pool
    :<|> protectedHandler
    :<|> canvasServer pool
    :<|> noteServer pool channelMap
    :<|> streamHandler jwtSettings pool channelMap

startApp :: IO ()
startApp = do
  jwtSettings <- initJWTSettings
  pool <- initConnectionPool connectionString
  channelMap <- newTVarIO Map.empty :: IO (TVar TopicChannelMap)
  let cfg = jwtSettings :. defaultCookieSettings :. EmptyContext
  putStrLn "Running server on port 8081"
  run 8081 $ simpleCors $ serveWithContext api cfg (server jwtSettings pool channelMap)