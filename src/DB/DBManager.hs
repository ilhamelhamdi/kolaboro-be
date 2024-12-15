module DB.DBManager (initConnectionPool, getConnectionString) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Pool
import Database.PostgreSQL.Simple
import System.Environment (getEnv)
import Control.Exception.Base

type DBConnectionString = ByteString

initConnectionPool :: DBConnectionString -> IO (Pool Connection)
initConnectionPool connStr =
  createPool
    (connectPostgreSQL connStr)
    close
    1 -- Number of sub-pools (stripes)
    60 -- The amount of seconds for which an unused resource is kept around
    10 -- The maximum number of resources to keep open across all stripes

getConnectionString :: IO DBConnectionString
getConnectionString = do
  dbUrl <- getEnv "DATABASE_URL"
  return $ pack dbUrl
  `catch` \e -> do
    putStrLn "Error: DATABASE_URL environment variable not set."
    throwIO (e :: IOException)
