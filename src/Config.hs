{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}

module Config
  ( Config(Config, port, defaultStation, maxDeparturesPerDirection)
  , loadConfig
  ) where

import Protolude
import qualified Dhall

data Config = Config
  { port :: Integer
  , defaultStation :: Dhall.Text
  , maxDeparturesPerDirection :: Integer
  } deriving (Generic, Show, Dhall.Interpret)

loadConfig :: IO Config
loadConfig = Dhall.input Dhall.auto "./config/bot.dhall"
