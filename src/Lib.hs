{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib (startApp) where

import API.Auth.Main (authServer)
import API.Canvas.Main (canvasServer)
import API.Note.Main (noteServer)
import API.Profile.Main (profileServer)
import API.Protected (protectedHandler)
import API.Root (API)
import API.Stream.Main (streamHandler)
import DB.DBManager
import qualified Data.Map as Map
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import GHC.Conc (TVar, newTVarIO)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Servant
import Servant.Auth.Server (JWTSettings, defaultCookieSettings)
import Utils.ChannelUtils (TopicChannelMap)
import Utils.CorsUtils (corsConfig)
import Utils.JWTUtils (initJWTSettings)

api :: Proxy API
api = Proxy

server :: JWTSettings -> Pool Connection -> TVar TopicChannelMap -> Server API
server jwtSettings pool channelMap =
  authServer jwtSettings pool
    :<|> protectedHandler
    :<|> canvasServer pool channelMap
    :<|> noteServer pool channelMap
    :<|> streamHandler jwtSettings pool channelMap
    :<|> profileServer pool

startApp :: IO ()
startApp = do
  jwtSettings <- initJWTSettings
  connectionString <- getConnectionString
  pool <- initConnectionPool connectionString
  channelMap <- newTVarIO Map.empty :: IO (TVar TopicChannelMap)
  let cfg = jwtSettings :. defaultCookieSettings :. EmptyContext
  putStrLn "Running server on port 8081"
  let app = logStdoutDev $ corsConfig $ serveWithContext api cfg (server jwtSettings pool channelMap)
  run 8081 app
