{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TransportApi () where

import Protolude hiding ((<>))
import Data.Text.Format as Format

import qualified Config

stationUrl :: Format.Format
stationUrl = "https://transportapi.com/v3/uk/tube/platforms/hammersmith/{stationName}.json?app_id={appId}&app_key={appKey}"

mkStationUrl :: Config.Config -> Text
mkStationUrl Config.Config{Config.stationName, Config.appId, Config.appKey} =
  toStrict $ Format.format stationUrl (stationName, appId, appKey)
