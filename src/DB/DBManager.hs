module DB.DBManager (initConnectionPool, connectionString) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Pool
import Database.PostgreSQL.Simple

type DBConnectionString = ByteString

initConnectionPool :: DBConnectionString -> IO (Pool Connection)
initConnectionPool connStr =
  createPool
    (connectPostgreSQL connStr)
    close
    1 -- Number of sub-pools (stripes)
    60 -- The amount of seconds for which an unused resource is kept around
    10 -- The maximum number of resources to keep open across all stripes

connectionString :: DBConnectionString
connectionString = pack "host=localhost port=5432 user=kolaboro password=kolaboroadmin dbname=kolaboro"