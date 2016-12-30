{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Protolude hiding ((<>))
import Data.Monoid ((<>))

-- Operator imports
import Servant ((:>))
import Control.Lens.Operators ((<&>))
import Data.Aeson ((.:?))

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HMS
import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant

import qualified Api
import qualified Common
import qualified Config
import qualified Response
import qualified VersionInfo

-- * Webhook Fulfillment Server

-- | Partial definition of the request payload
data WebhookRequest = WebhookRequest
  { _result :: WebhookResult
  , _id :: Text
  } deriving (Generic, Show)

instance Aeson.FromJSON WebhookRequest where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions { Aeson.fieldLabelModifier = drop 1 }

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

data WebhookParameters = WebhookParameters
  { _direction :: Maybe Common.Direction
  , _station :: Maybe Text
  , _line :: Maybe Text
  } deriving (Generic, Show)

instance Aeson.FromJSON WebhookParameters where
  parseJSON = Aeson.withObject "parameters" $ \o -> do
    _direction <- o .:? "direction"
    _station <- o .:?! "station"
    _line <- o .:?! "line"
    return WebhookParameters{..}

(.:?!)
  :: Aeson.Object
  -> Text
  -> Aeson.Parser (Maybe Text)
obj .:?! key =
  case HMS.lookup key obj of
    Nothing -> pure Nothing
    Just "" -> pure Nothing
    Just v -> Aeson.parseJSON v
{-# INLINE (.:?!) #-}

-- API specification
type Api = "webhook"
           :> Servant.ReqBody '[Servant.JSON] WebhookRequest
           :> Servant.Post '[Servant.JSON] Common.WebhookFulfillment

api :: Proxy Api
api = Proxy

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'Handler' monad.
server :: Config.Config -> Servant.Server Api
server c = postWebhookH

  -- This isn't a great Monad to work in. I want better error handling.
  where postWebhookH :: MonadIO m => WebhookRequest -> m Common.WebhookFulfillment
        postWebhookH wh =
          case _action . _result $ wh of
            ActionListDepartures -> listDeparturesH wh
            ActionAbout -> aboutH
            ActionUndefined -> undefinedH

        listDeparturesH :: MonadIO m => WebhookRequest -> m Common.WebhookFulfillment
        listDeparturesH wh =
          Ex.runExceptT (fulfillDepartureReq c wh)
            <&> either (\(Common.FulfillmentError e) -> Common.mkFulfillment e) identity

        aboutH :: Monad m => m Common.WebhookFulfillment
        aboutH = pure . Common.mkFulfillment $
             "I'm Passy's Tube Bot version "
          <> VersionInfo.programVersion
          <> ", running on "
          <> VersionInfo.compilerVersionName
          <> "."

        undefinedH :: Monad m => m Common.WebhookFulfillment
        undefinedH = pure $ Common.mkFulfillment "Sorry, I don't know how to help with that."

whenIsJust
  :: Monad m
  => Maybe a
  -> (a -> m ())
  -> m ()
whenIsJust (Just x) f = f x
whenIsJust Nothing _ = return ()

fulfillDepartureReq
  :: (MonadIO m)
  => Config.Config
  -> WebhookRequest
  -> m Common.WebhookFulfillment
fulfillDepartureReq c wh = do
  let params = _parameters . _result $ wh
  let dir' = fromMaybe Common.Spellbound $ _direction params
  let station' = _station params
  res <- Api.loadDeparturesForStation c station'

  return . Response.runResponse c $ do
    maybe (Response.abort $ Common.FulfillmentError "Sorry, I couldn't find any trains right now.")
      (Response.departures)
      res

    whenIsJust (_line params) Response.line
    whenIsJust (station') Response.station
    Response.direction dir'

-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
app :: Config.Config -> Servant.Application
app c = Servant.serve api (server c)

-- Run the server.
--
-- 'run' comes from Network.Wai.Handler.Warp
runServer :: Config.Config -> IO ()
runServer c = Warp.run (fromIntegral $ Config.port c) (app c)

-- Put this all to work!
main :: IO ()
main = do
  config <- Config.loadConfig
  putStrLn @Text $ "Starting server at http://localhost:" <> show (Config.port config) <> " ..."
  runServer config
