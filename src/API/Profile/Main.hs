{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Profile.Main (ProfileAPI, profileServer) where

import API.Profile.GetProfile (GetProfileAPI, getProfileHandler)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Servant

type ProfileAPI = "profile" :> GetProfileAPI

profileServer :: Pool Connection -> Server ProfileAPI
profileServer = getProfileHandler