{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Protolude hiding ((<>))
import Data.Monoid ((<>))

-- Operator imports
import Servant ((:>))
import Control.Lens.Operators ((<&>))

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import qualified Control.Monad.Except as Ex
import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant

import qualified Config
import qualified Common
import qualified Api

-- * Webhook Fulfillment Server

-- | Partial definition of the request payload
data WebhookRequest = WebhookRequest
  { _result :: WebhookResult
  , _id :: Text
  } deriving (Generic, Show)

instance Aeson.FromJSON WebhookRequest where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

-- | Wraps a text that is sent to the user in case of an error and might be logged
-- separately.
newtype FulfillmentError = FulfillmentError Text

data WebhookAction
  = ActionListDepartures
  | ActionUndefined
  deriving (Generic, Show)

instance Aeson.FromJSON WebhookAction where
  parseJSON (Aeson.String a)
    | a == "list-departures" = return ActionListDepartures
  parseJSON _ = return ActionUndefined

data WebhookResult = WebhookResult
  { _action :: WebhookAction
  , _parameters :: WebhookParameters
  } deriving (Generic, Show)

instance Aeson.FromJSON WebhookResult where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

newtype WebhookParameters =
  WebhookParameters (HMS.HashMap Text Text)
  deriving (Generic, Show, Aeson.FromJSON)

data WebhookFulfillment = WebhookFulfilment
  { _speech :: Text
  , _displayText :: Text
  , _source :: Text
  } deriving (Generic, Show)

instance Aeson.ToJSON WebhookFulfillment where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

mkFulfillment :: Text -> WebhookFulfillment
mkFulfillment text = WebhookFulfilment text text "tube-bot-fulfillment"

-- API specification
type Api = "webhook" :> Servant.ReqBody '[Servant.JSON] WebhookRequest :> Servant.Post '[Servant.JSON] WebhookFulfillment

testApi :: Proxy Api
testApi = Proxy

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'Handler' monad.
server :: Config.Config -> Servant.Server Api
server c = postWebhookH

  -- This isn't a great Monad to work in. I want better error handling.
  where postWebhookH :: MonadIO m => WebhookRequest -> m WebhookFulfillment
        postWebhookH wh =
          Ex.runExceptT (fulfillDepartureReq c wh) <&> \case
            Left (FulfillmentError e) -> mkFulfillment e
            Right f -> f

fulfillDepartureReq
  :: (MonadIO m, Ex.MonadError FulfillmentError m)
  => Config.Config
  -> WebhookRequest
  -> m WebhookFulfillment
fulfillDepartureReq c wh = do
  res <- Api.loadDeparturesForStation c $ Just "AldgateEast"
  case res of
    Just res' -> filterDepartures res'
    Nothing -> Ex.throwError $ FulfillmentError "Sorry, I couldn't resolve any trains right now."

  where
    filterDepartures
      :: (Ex.MonadError FulfillmentError m)
      => Api.DepartureMap
      -> m WebhookFulfillment
    filterDepartures (Api.DepartureMap d) =
      return $ case HMS.lookup Api.Westbound d of
        -- We found departures for the specified direction.
        Just departures -> return $ formatDepartures Api.Westbound departures
        -- We can't filter by direction, so we'll list them all.
        Nothing -> Ex.throwError $ FulfillmentError "Sorry, I couldn't find any westbound train departures right now."

formatDepartures :: Common.Direction -> [Api.Departure] -> WebhookFulfillment
formatDepartures direction ds =
  let preamble :: Text
      preamble = "I found the following " <> Common.formatDirection direction <> " departures from Aldgate East: "
      format d = Api.departureLine d
              <> " to "
              <> Api.departureDestination d
              <> " in "
              <> formatSeconds (Api.departureSeconds d)
              <> " seconds."
      body = format <$> ds
  in mkFulfillment $ preamble <> T.unwords body

formatSeconds :: Num n => n -> Text
formatSeconds n | n < 60 = "less than a minute"
                | n <= 90 = show n <> " seconds"
                | otherwise = round (n / 60) <> " minutes"

-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
app :: Config.Config -> Servant.Application
app c = Servant.serve testApi (server c)

-- Run the server.
--
-- 'run' comes from Network.Wai.Handler.Warp
runTestServer :: Config.Config -> IO ()
runTestServer c = Warp.run (fromIntegral $ Config.port c) (app c)

-- Put this all to work!
main :: IO ()
main = do
  config <- Config.loadConfig
  putStrLn ("Starting server at http://localhost:" <> show (Config.port config) <> " ..." :: Text)
  runTestServer config
