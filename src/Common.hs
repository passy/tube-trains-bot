{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Common
  ( Direction(..)
  , formatDirection
  ) where

import Protolude hiding ((<>))
import qualified Data.Aeson as Aeson
import qualified Data.Hashable as Hashable
import qualified Data.Text as T

data Direction
  = Westbound
  | Eastbound
  | Northbound
  | Southbound
  | Spellbound
  deriving (Show, Eq, Generic, Hashable.Hashable)

instance Aeson.FromJSON Direction where
  parseJSON (Aeson.String a) =
    pure $ case T.toLower a of
      "westbound" -> Westbound
      "eastbound" -> Eastbound
      "northbound" -> Northbound
      "southbound" -> Northbound
      _ -> Spellbound

  parseJSON _ = mempty

-- | Format a Direction to be read out or sent to a user.
formatDirection :: Direction -> Text
formatDirection = show
