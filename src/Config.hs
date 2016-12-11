{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}

module Config
  ( Config(appId, appKey, stationName, port)
  , loadConfig
  ) where

import Protolude
import qualified Dhall as Dhall

data Config = Config
  { appId :: Dhall.Text
  , appKey :: Dhall.Text
  , port :: Integer
  , stationName :: Dhall.Text
  } deriving (Generic, Show, Dhall.Interpret)

loadConfig :: IO Config
loadConfig = do
  Dhall.input Dhall.auto "./bot.dhall"
