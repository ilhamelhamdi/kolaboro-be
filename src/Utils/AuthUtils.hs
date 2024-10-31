{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Utils.AuthUtils (authCheck) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool, withResource)
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Connection, query)
import Model.User hiding (email, password)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated))

authCheck :: Pool Connection -> BasicAuthData -> IO (AuthResult User)
authCheck pool (BasicAuthData _email _password) = do
  liftIO $ withResource pool $ \conn -> do
    let q = "SELECT email, password, registered_date FROM users WHERE email=? AND password=?"
    users <- query conn q (_email, _password) :: IO [(String, String, UTCTime)]
    let (_email, _name, _registered_date) = head users
    return $ Authenticated $ User _email "" _name _registered_date