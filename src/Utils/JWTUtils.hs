module Utils.JWTUtils (generateToken, generateKey, initJWTSettings) where

import Crypto.JOSE.JWK (JWK, KeyMaterialGenParam (RSAGenParam), genJWK)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Model.User (User)
import Servant.Auth.Server (JWTSettings, defaultJWTSettings, makeJWT)

generateKey :: IO JWK
generateKey = genJWK (RSAGenParam 2048)

initJWTSettings :: IO JWTSettings
initJWTSettings = defaultJWTSettings <$> generateKey

generateToken :: JWTSettings -> User -> IO (Either String Text)
generateToken jwtSettings user = do
  currentTime <- getCurrentTime
  let expirationTime = addUTCTime 3600 currentTime -- Token valid for 1 hour
  eJwt <- makeJWT user jwtSettings Nothing
  case eJwt of
    Left err -> return $ Left (show err)
    Right jwt -> return $ Right (decodeUtf8 $ BL.toStrict jwt)
