{-# LANGUAGE DataKinds #-}

module Lib (startApp) where

import API.Auth.Main (authServer)
import API.Root (API)
import API.User (userServer)
import DB.DBManager
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Auth.Server (JWTSettings)
import Utils.JWTUtils (initJWTSettings)

api :: Proxy API
api = Proxy

server :: JWTSettings -> Pool Connection -> Server API
server jwtSettings pool = userServer :<|> authServer jwtSettings pool

startApp :: IO ()
startApp = do
  jwtSettings <- initJWTSettings
  pool <- initConnectionPool connectionString
  putStrLn "Running server on port 8081"
  run 8081 $ serve api (server jwtSettings pool)