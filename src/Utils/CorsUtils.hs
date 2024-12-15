{-# LANGUAGE OverloadedStrings #-}

module Utils.CorsUtils (corsConfig) where

import Network.Wai
import Network.Wai.Middleware.Cors

corsResourcePolicy :: CorsResourcePolicy
corsResourcePolicy =
  CorsResourcePolicy
    { corsOrigins = Just (["http://localhost:3000", "https://kolaboro-fe.vercel.app/"], True),
      corsMethods = simpleMethods ++ ["OPTIONS", "PUT", "DELETE"],
      corsRequestHeaders = ["content-type", "Authorization"],
      corsExposedHeaders = Nothing,
      corsMaxAge = Nothing,
      corsVaryOrigin = False,
      corsRequireOrigin = False,
      corsIgnoreFailures = False
    }

corsConfig :: Middleware
corsConfig = cors (const $ Just corsResourcePolicy)