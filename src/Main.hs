{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Protolude hiding ((<>))
import Data.Monoid ((<>))

-- Operator imports
import Servant ((:>))
import Control.Lens (at)
import Control.Lens.Iso (non)
import Control.Lens.Operators ((<&>), (?=), (<>~))
import Control.Lens.TH (makeLenses)
import Data.Aeson ((.:?))

import qualified Control.Monad.Free as Free
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Char as Char
import qualified Data.Default as Def
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
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
type Api = "webhook"
           :> Servant.ReqBody '[Servant.JSON] WebhookRequest
           :> Servant.Post '[Servant.JSON] WebhookFulfillment

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
          case _action . _result $ wh of
            ActionListDepartures -> listDeparturesH wh
            ActionAbout -> aboutH
            ActionUndefined -> undefinedH

        listDeparturesH :: MonadIO m => WebhookRequest -> m WebhookFulfillment
        listDeparturesH wh =
          Ex.runExceptT (fulfillDepartureReq c wh)
            <&> either (\(FulfillmentError e) -> mkFulfillment e) identity

        aboutH :: Monad m => m WebhookFulfillment
        aboutH = pure . mkFulfillment $
             "I'm Passy's Tube Bot version "
          <> VersionInfo.programVersion
          <> ", running on "
          <> VersionInfo.compilerVersionName
          <> "."

        undefinedH :: Monad m => m WebhookFulfillment
        undefinedH = pure $ mkFulfillment "Sorry, I don't know how to help with that."

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
    filterDepartures dir mline d =
      -- Filter by line if filter is provided
      let d' = HMS.mapMaybe (maybe pure filterLine mline) d
      in
        return . mkFulfillment $ case (null d', HMS.lookup dir d') of
          (True, _) -> "I could not find any departures for the given parameters."
          -- We found departures for the specified direction.
          (False, Just departures) -> formatDepartures c dir departures
          -- We can't filter by direction, so we'll list them all.
          (False, Nothing) ->
            let go m k v = formatDepartures c k v : m
                l = HMS.foldlWithKey' go empty d'
            in T.unwords l

formatDepartures :: Config.Config -> Common.Direction -> [Api.Departure] -> Text
formatDepartures _ Common.Spellbound [] =
  "Sorry, there don't seem to be any departures from this station at the moment."
formatDepartures _ direction [] =
     "Sorry, there don't seem to be any "
  <> Common.formatDirection direction
  <> " departures from this station at the moment."
formatDepartures c direction ds =
  let directionTxt :: [Text]
      directionTxt = if direction == Common.Spellbound then [] else pure (Common.formatDirection direction)
      preamble :: [Text]
      preamble = [ "I found the following" ] ++ directionTxt ++ [ "departures from Aldgate East:" ]
      format d = unCamelCase (Api.departureLine d)
              <> " line to "
              <> Api.departureDestination d
              <> " in "
              <> formatSeconds (Api.departureSeconds d)
              <> "."
      body = format <$> take (fromIntegral $ Config.maxDeparturesPerDirection c) ds
  in T.unwords $ preamble ++ body

formatSeconds :: Int -> Text
formatSeconds n | n < 60 = "less than a minute"
                | n <= 90 = show n <> " seconds"
                | otherwise = show (quot n 60) <> " minutes"

-- | Turn a camel-cased string into a whitespace separted string.
-- Does manual unpacking and repacking, because I'm a terrible
-- programmer, hence very inefficient.
--
-- >>> unCamelCase "HammersmithAndCity"
-- "Hammersmith And City"
-- >>> unCamelCase "MyURIParser"
-- "My URI Parser"
unCamelCase :: Text -> Text
unCamelCase = T.pack . go . T.unpack
  where go (c:d:cs) | Char.isUpper d && not (Char.isUpper c) = c : ' ' : go (d:cs)
                    | otherwise = c : go (d:cs)
        go cs = cs

-- * Fulfillment response combinators

data ResponseF r =
    AbortF Text
  | DepartureF Common.Direction Api.Departure r
  deriving (Show, Functor)

type Response = Free.Free ResponseF

abort
  :: Text
  -> Response ()
abort res = Free.liftF $ AbortF res

departure
  :: Common.Direction
  -> Api.Departure
  -> Response ()
departure dir dep = Free.liftF $ DepartureF dir dep ()

exampleProc :: Response ()
exampleProc = do
  departure Common.Westbound (Api.Departure "Passy Line" "Passy's Basement" 120)
  departure Common.Eastbound (Api.Departure "District" "Richmond" 300)

-- TODO: Move me.
data ResponseState = ResponseState
  { _sError :: Maybe Text
  , _sStation :: Maybe Text
  , _sDepartures :: Api.DepartureMap
  , _sPointOfDeparture :: Maybe Text }
  deriving (Show)

makeLenses ''ResponseState

instance Def.Default ResponseState where
  def = ResponseState mempty mempty mempty mempty

runResponse :: Response () -> WebhookFulfillment
runResponse res = go $ (State.execState (interpResponse res) Def.def)
  where go (s@ResponseState{..}) = mkFulfillment $ show s

interpResponse :: Response () -> State.State ResponseState ()
interpResponse (Free.Free (AbortF err)) = sError ?= err
interpResponse (Free.Free (DepartureF dir dep r)) =
  -- This warrents a bit of documentation:
  -- `sDepartures` is the lens obviously, `at dir` works on the hashmap but gives
  -- us a Maybe, using a _Just prism would work, but then we wouldn't append if it was
  -- Nothing, so we want to supply a default value, which is exactly what the `non` iso
  -- does. `<>~` `mappend`s a new value and even sets it to Just if neccessary.
  -- Cool? Cool.
  State.modify (sDepartures . at dir . non [] <>~ pure dep) >> interpResponse r
interpResponse (Free.Pure _) =
  return ()

-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
app :: Config.Config -> Servant.Application
app c = Servant.serve testApi (server c)

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
