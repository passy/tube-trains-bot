{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}

module Config
  ( Config(Config, port, maxDeparturesPerDirection)
  , loadConfig
  ) where

import Protolude
import qualified Dhall
import Numeric.Natural (Natural)

data Config = Config
  { port :: Natural
  , maxDeparturesPerDirection :: Natural
  } deriving (Generic, Show, Dhall.Interpret)

loadConfig :: IO Config
loadConfig = Dhall.input Dhall.auto "./config/bot.dhall"
