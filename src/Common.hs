{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Common
  ( Direction(..)
  , StationName(StationName)
  , FulfillmentError(..)
  , WebhookFulfillment(..)
  , mkFulfillment
  , formatDirection
  ) where

import Protolude hiding ((<>))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Hashable as Hashable
import qualified Data.Text as T

-- * ADTs and type wrappers

newtype StationName = StationName Text
  deriving (Show, Eq)

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

-- | Wraps a text that is sent to the user in case of an error and might be logged
-- separately.
newtype FulfillmentError = FulfillmentError Text
  deriving (Show)

-- | The data type to return to API.ai which is currently speech-only.
data WebhookFulfillment = WebhookFulfillment
  { _speech :: Text
  , _displayText :: Text
  , _source :: Text
  } deriving (Generic, Show)

instance Aeson.ToJSON WebhookFulfillment where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

mkFulfillment :: Text -> WebhookFulfillment
mkFulfillment text = WebhookFulfillment text text "tube-bot-fulfillment"
