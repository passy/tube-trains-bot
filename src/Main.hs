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

import Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HMS
import Data.Monoid
import Data.Text
import Network.Wai
import Network.Wai.Handler.Warp

import Servant

-- * Webhook Fulfillment Server

-- | Partial definition of the request payload
data WebhookRequest = WebhookRequest
  { result :: WebhookResult
  , id :: Text
  } deriving (Generic, Show, FromJSON)

data WebhookAction
  = ActionListDepartures
  | ActionUndefined
  deriving (Generic, Show)

instance Aeson.FromJSON WebhookAction where
  parseJSON (Aeson.String a)
    | a == "list-departures" = return ActionListDepartures
  parseJSON _ = return ActionUndefined

data WebhookResult = WebhookResult
  { action :: Text
  , parameters :: WebhookParameters
  } deriving (Generic, Show, FromJSON)

newtype WebhookParameters =
  WebhookParameters (HMS.HashMap Text Text)
  deriving (Generic, Show, FromJSON)

-- | A greet message data type
newtype Greet = Greet { _msg :: Text }
  deriving (Generic, Show, FromJSON, ToJSON)

-- API specification
type TestApi =
       -- GET /hello/:name?capital={true, false}  returns a Greet as JSON
       "hello" :> Capture "name" Text :> QueryParam "capital" Bool :> Get '[JSON] Greet

       -- POST /greet with a Greet as JSON in the request body,
       --             returns a Greet as JSON
  :<|> "greet" :> ReqBody '[JSON] Greet :> Post '[JSON] Greet

       -- Temporary
  :<|> "webhook" :> ReqBody '[JSON] WebhookRequest :> Post '[JSON] Text

       -- DELETE /greet/:greetid
  :<|> "greet" :> Capture "greetid" Text :> Delete '[JSON] NoContent

testApi :: Proxy TestApi
testApi = Proxy

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'Handler' monad.
server :: Server TestApi
server = helloH :<|> postGreetH :<|> postWebhookH :<|> deleteGreetH

  where helloH name Nothing = helloH name (Just False)
        helloH name (Just False) = return . Greet $ "Hello, " <> name
        helloH name (Just True) = return . Greet . toUpper $ "Hello, " <> name

        postGreetH greet = return greet

        postWebhookH wh = return $ id wh

        deleteGreetH _ = return NoContent

-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
test :: Application
test = serve testApi server

-- Run the server.
--
-- 'run' comes from Network.Wai.Handler.Warp
runTestServer :: Port -> IO ()
runTestServer port = run port test

-- Put this all to work!
main :: IO ()
main = runTestServer 8001
