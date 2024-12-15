{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module DTO.ResponseDTO (ResponseDTO, successResponse, errorResponse, jsonError, jsonError401, jsonError403, jsonError404) where

import Data.Aeson
import GHC.Generics (Generic)
import Servant hiding (Header)

data ResponseDTO a = ResponseDTO
  { status :: String,
    message :: String,
    data_ :: Maybe a,
    error_ :: Maybe String
  }
  deriving (Eq, Show, Generic)

instance (ToJSON a) => ToJSON (ResponseDTO a) where
  toJSON (ResponseDTO _status _message _data _error) =
    object
      [ "status" .= _status,
        "message" .= _message,
        "data" .= _data,
        "error" .= _error
      ]

successResponse :: String -> Maybe a -> ResponseDTO a
successResponse msg maybeData =
  ResponseDTO {status = "success", message = msg, error_ = Nothing, data_ = maybeData}

errorResponse :: String -> Maybe String -> ResponseDTO String
errorResponse msg maybeError =
  ResponseDTO {status = "error", message = msg, error_ = maybeError, data_ = Nothing}

jsonError :: ServerError -> String -> Maybe String -> ServerError
jsonError errType msg errMsg = errType {errBody = encode $ errorResponse msg errMsg, errHeaders = [("Content-Type", "application/json")]}

jsonError401 :: String -> Maybe String -> ServerError
jsonError401 = jsonError err401

jsonError403 :: String -> Maybe String -> ServerError
jsonError403 = jsonError err403

jsonError404 :: String -> Maybe String -> ServerError
jsonError404 = jsonError err404