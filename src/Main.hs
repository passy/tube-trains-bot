{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Protolude hiding ((<>))
import Data.Monoid ((<>))

-- Operator imports
import Servant ((:<|>)((:<|>)), (:>))

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant

import qualified Config
import qualified Api

-- * Webhook Fulfillment Server

-- | Partial definition of the request payload
data WebhookRequest = WebhookRequest
  { result :: WebhookResult
  , id :: Text
  } deriving (Generic, Show, Aeson.FromJSON)

data WebhookAction
  = ActionListDepartures
  | ActionUndefined
  deriving (Generic, Show)

instance Aeson.FromJSON WebhookAction where
  parseJSON (Aeson.String a)
    | a == "list-departures" = return ActionListDepartures
  parseJSON _ = return ActionUndefined

data WebhookResult = WebhookResult
  { action :: WebhookAction
  , parameters :: WebhookParameters
  } deriving (Generic, Show, Aeson.FromJSON)

newtype WebhookParameters =
  WebhookParameters (HMS.HashMap Text Text)
  deriving (Generic, Show, Aeson.FromJSON)

-- | A greet message data type
newtype Greet = Greet { _msg :: Text }
  deriving (Generic, Show, Aeson.FromJSON, Aeson.ToJSON)

-- API specification
type TestApi =
       -- GET /hello/:name?capital={true, false}  returns a Greet as JSON
       "hello" :> Servant.Capture "name" Text :> Servant.QueryParam "capital" Bool :> Servant.Get '[Servant.JSON] Greet

       -- POST /greet with a Greet as JSON in the request body,
       --             returns a Greet as JSON
  :<|> "greet" :> Servant.ReqBody '[Servant.JSON] Greet :> Servant.Post '[Servant.JSON] Greet

       -- Temporary
  :<|> "webhook" :> Servant.ReqBody '[Servant.JSON] WebhookRequest :> Servant.Post '[Servant.JSON] Text

       -- DELETE /greet/:greetid
  :<|> "greet" :> Servant.Capture "greetid" Text :> Servant.Delete '[Servant.JSON] Servant.NoContent

testApi :: Proxy TestApi
testApi = Proxy

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'Handler' monad.
server :: Config.Config -> Servant.Server TestApi
server c = helloH :<|> postGreetH :<|> postWebhookH :<|> deleteGreetH

  where helloH :: Monad m => Text -> Maybe Bool -> m Greet
        helloH name Nothing = helloH name (Just False)
        helloH name (Just False) = return . Greet $ "Hello, " <> name
        helloH name (Just True) = return . Greet . T.toUpper $ "Hello, " <> name

        postGreetH :: Monad m => a -> m a
        postGreetH greet = return greet

        postWebhookH :: MonadIO m => WebhookRequest -> m Text
        postWebhookH wh = do
          res <- Api.loadDeparturesForStation c (Just "AldgateEast")
          print res
          return $ id wh

        deleteGreetH :: Monad m => a -> m Servant.NoContent
        deleteGreetH _ = return Servant.NoContent

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
  putStrLn $ ("Starting server at http://localhost:" <> show (Config.port config) <> " ..." :: Text)
  runTestServer config
