{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Common (Direction(..), formatDirection) where

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
    case T.toLower a of
      "Westbound" -> return Westbound
      "Eastbound" -> return Eastbound
      "Northbound" -> return Northbound
      "Southbound" -> return Northbound
      _ -> return Spellbound

  parseJSON _ = mempty

-- | Format a Direction to be read out or sent to a user.
formatDirection :: Direction -> Text
formatDirection = show
