{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Utils.JWTUtils (generateToken, generateKey, initJWTSettings, UserClaims (..), userToUserClaims, getUserFromUserClaims) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Crypto.JWT
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as BL
import Data.Pool (Pool)
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time (addUTCTime, getCurrentTime)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import Model.User (User)
import qualified Model.User as User
import Repo.BaseRepo (BaseRepo (findById), PGRepo (PGRepo))
import Servant.Auth.Server (FromJWT, JWTSettings, ToJWT, defaultJWTSettings, makeJWT)

generateKey :: IO JWK
generateKey = do
  putStrLn "Generating JWK key"
  key <- genJWK (RSAGenParam 256)
  putStrLn "JWK key generated"
  return key

initJWTSettings :: IO JWTSettings
initJWTSettings = do
  putStrLn "Initializing JWT settings"
  key <- generateKey
  putStrLn "JWT settings initialized"
  return $ defaultJWTSettings key

generateToken :: JWTSettings -> User -> IO (Either String Text)
generateToken jwtSettings user = do
  currentTime <- getCurrentTime
  let expirationTime = addUTCTime 3600 currentTime -- Token valid for 1 hour
  eJwt <- makeJWT (userToUserClaims user) jwtSettings (Just expirationTime)
  case eJwt of
    Left err -> return $ Left (show err)
    Right jwt -> return $ Right (decodeUtf8 $ BL.toStrict jwt)

data UserClaims = UserClaims
  { userId :: Int,
    email :: String,
    username :: String,
    displayName :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON UserClaims

instance FromJSON UserClaims

instance ToJWT UserClaims

instance FromJWT UserClaims

userToUserClaims :: User -> UserClaims
userToUserClaims user = UserClaims {userId = User.id user, email = User.email user, username = User.username user, displayName = User.display_name user}

getUserFromUserClaims :: Pool Connection -> UserClaims -> IO (Maybe User)
getUserFromUserClaims pool userClaims = liftIO $ findById (PGRepo pool :: PGRepo User) (userId userClaims)