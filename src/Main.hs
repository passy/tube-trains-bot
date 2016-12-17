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
import qualified VersionInfo

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
  | ActionAbout
  | ActionUndefined
  deriving (Generic, Show)

instance Aeson.FromJSON WebhookAction where
  parseJSON (Aeson.String a)
    | a == "list-departures" = return ActionListDepartures
    | a == "about" = return ActionAbout
  parseJSON _ = return ActionUndefined

data WebhookResult = WebhookResult
  { _action :: WebhookAction
  , _parameters :: WebhookParameters
  } deriving (Generic, Show)

instance Aeson.FromJSON WebhookResult where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

-- TODO: Figure out if we can easily turn those into `Nothing` if they're empty.
data WebhookParameters = WebhookParameters
  { _direction :: Maybe Common.Direction
  , _station :: Maybe Text
  , _line :: Maybe Text
  } deriving (Generic, Show)

instance Aeson.FromJSON WebhookParameters where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

data WebhookFulfillment = WebhookFulfillment
  { _speech :: Text
  , _displayText :: Text
  , _source :: Text
  } deriving (Generic, Show)

instance Aeson.ToJSON WebhookFulfillment where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

mkFulfillment :: Text -> WebhookFulfillment
mkFulfillment text = WebhookFulfillment text text "tube-bot-fulfillment"

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
          case (_action . _result $ wh) of
            ActionListDepartures -> listDeparturesH wh
            ActionAbout -> aboutH
            ActionUndefined -> undefinedH

        listDeparturesH :: MonadIO m => WebhookRequest -> m WebhookFulfillment
        listDeparturesH wh =
          Ex.runExceptT (fulfillDepartureReq c wh) <&> \case
            Left (FulfillmentError e) -> mkFulfillment e
            Right f -> f

        aboutH :: Monad m => m WebhookFulfillment
        aboutH = pure . mkFulfillment $
             "I'm Passy's Tube Bot version "
          <> VersionInfo.programVersion
          <> ", running on "
          <> VersionInfo.compilerVersionName
          <> "."

        undefinedH :: Monad m => m WebhookFulfillment
        undefinedH = pure $ mkFulfillment $ "Sorry, I don't know how to help with that."

fulfillDepartureReq
  :: (MonadIO m, Ex.MonadError FulfillmentError m)
  => Config.Config
  -> WebhookRequest
  -> m WebhookFulfillment
fulfillDepartureReq c wh = do
  let params = _parameters . _result $ wh
  let dir = fromMaybe Common.Spellbound $ _direction params
  let line = _line params
  let station = _station params
  res <- Api.loadDeparturesForStation c station
  case res of
    Just res' -> filterDepartures dir line res'
    Nothing -> Ex.throwError $ FulfillmentError "Sorry, I couldn't find any trains right now."
  where
    filterLine
      :: Text
      -> [Api.Departure]
      -> Maybe [Api.Departure]
    filterLine line ds =
      let res = filter (\d -> Api.departureLine d == line) ds
      in if null res
           then Nothing
           else Just res

    filterDepartures
      :: (Ex.MonadError FulfillmentError m)
      => Common.Direction
      -> Maybe Text
      -> Api.DepartureMap
      -> m WebhookFulfillment
    filterDepartures dir mline (Api.DepartureMap d) =
      -- Filter by line if filter is provided
      let d' = HMS.mapMaybe (maybe pure filterLine mline) d
      in
        return . mkFulfillment $ case HMS.lookup dir d' of
          -- We found departures for the specified direction.
          Just departures -> formatDepartures dir departures
          -- We can't filter by direction, so we'll list them all.
          Nothing ->
            let go m k v = formatDepartures k v : m
                l = HMS.foldlWithKey' go empty d'
            in T.unwords l

formatDepartures :: Common.Direction -> [Api.Departure] -> Text
formatDepartures direction ds =
  let preamble :: Text
      preamble = "I found the following " <> Common.formatDirection direction <> " departures from Aldgate East: "
      format d = Api.departureLine d
              <> " to "
              <> Api.departureDestination d
              <> " in "
              <> formatSeconds (Api.departureSeconds d)
              <> "."
      body = format <$> ds
  in preamble <> T.unwords body

formatSeconds :: Int -> Text
formatSeconds n | n < 60 = "less than a minute"
                | n <= 90 = show n <> " seconds"
                | otherwise = show (quot n 60) <> " minutes"

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
