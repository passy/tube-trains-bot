{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Response where

import Protolude hiding ((<>))
import Data.Monoid ((<>))

-- Operator imports
import Control.Lens (at)
import Control.Lens.Iso (non)
import Control.Lens.Operators ((.=), (?=), (<>~))
import Control.Lens.TH (makeLenses)

import qualified Control.Monad.State.Strict as State
import qualified Control.Monad.Free as Free
import qualified Data.Char as Char
import qualified Data.Default as Def
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T

import qualified Config
import qualified Common
import qualified Api

-- * Fulfillment response combinators

data ResponseF r =
    AbortF Common.FulfillmentError
  | LineF Text r
  | StationF Text r
  | DirectionF Common.Direction r
  | DepartureF Common.Direction Api.Departure r
  | DeparturesF Api.DepartureMap r
  deriving (Show, Functor)

type Response = Free.Free ResponseF

abort
  :: Common.FulfillmentError
  -> Response ()
abort = Free.liftF . AbortF

departure
  :: Common.Direction
  -> Api.Departure
  -> Response ()
departure dir dep = Free.liftF $ DepartureF dir dep ()

direction
  :: Common.Direction
  -> Response ()
direction d = Free.liftF $ DirectionF d ()

departures
  :: Api.DepartureMap
  -> Response ()
departures dm = Free.liftF $ DeparturesF dm ()

line
  :: Text
  -> Response ()
line txt = Free.liftF $ LineF txt ()

station
  :: Text
  -> Response ()
station txt = Free.liftF $ StationF txt ()

-- TODO: Move me.
data ResponseState = ResponseState
  { _sError :: Maybe Common.FulfillmentError
  , _sStation :: Maybe Text
  , _sLine :: Maybe Text
  , _sDepartures :: Api.DepartureMap
  , _sDirection :: Common.Direction }
  deriving (Show)

makeLenses ''ResponseState

instance Def.Default ResponseState where
  def = ResponseState empty empty empty mempty Common.Spellbound

-- TODO: Remove config dependency from here
runResponse :: Config.Config -> Response () -> Common.WebhookFulfillment
runResponse c res = format $ State.execState (interp res) Def.def
  where
    format (ResponseState{..}) =
      case _sError of
        Just (Common.FulfillmentError err) -> Common.mkFulfillment err
        Nothing -> filterDepartures c _sDirection _sLine _sDepartures

interp :: Response () -> State.State ResponseState ()
interp (Free.Free (AbortF err)) = sError ?= err
interp (Free.Free (LineF v r)) = sLine ?= v >> interp r
interp (Free.Free (StationF v r)) = sStation ?= v >> interp r
interp (Free.Free (DirectionF v r)) = sDirection .= v >> interp r
interp (Free.Free (DepartureF dir dep r)) =
  -- This warrants a bit of documentation:
  -- `sDepartures` is the lens obviously, `at dir` works on the hashmap but gives
  -- us a Maybe, using a _Just prism would work, but then we wouldn't append if it was
  -- Nothing, so we want to supply a default value, which is exactly what the `non` iso
  -- does. `<>~` `mappend`s a new value and even sets it to Just if neccessary.
  -- Cool? Cool.
  State.modify (sDepartures . at dir . non [] <>~ pure dep) >> interp r
interp (Free.Free (DeparturesF v r)) =
  State.modify (sDepartures <>~ v) >> interp r
interp (Free.Pure _) =
  return ()

filterLine
  :: Text
  -> [Api.Departure]
  -> Maybe [Api.Departure]
filterLine l ds =
  let res = filter (\d -> Api.departureLine d == l) ds
  in if null res
        then Nothing
        else Just res

filterDepartures
  :: Config.Config
  -> Common.Direction
  -> Maybe Text
  -> Api.DepartureMap
  -> Common.WebhookFulfillment
filterDepartures c dir mline d =
  -- Filter by line if filter is provided
  let d' = HMS.mapMaybe (maybe pure filterLine mline) d
  in
    Common.mkFulfillment $ case (null d', HMS.lookup dir d') of
      (True, _) -> "I could not find any departures for the given parameters."
      -- We found departures for the specified direction.
      (False, Just ds) -> formatDepartures c dir ds
      -- We can't filter by direction, so we'll list them all.
      (False, Nothing) ->
        let go m k v = formatDepartures c k v : m
            l = HMS.foldlWithKey' go empty d'
        in T.unwords l

formatDepartures :: Config.Config -> Common.Direction -> [Api.Departure] -> Text
formatDepartures _ Common.Spellbound [] =
  "Sorry, there don't seem to be any departures from this station at the moment."
formatDepartures _ dir [] =
     "Sorry, there don't seem to be any "
  <> Common.formatDirection dir
  <> " departures from this station at the moment."
formatDepartures c dir ds =
  let directionTxt :: [Text]
      directionTxt = if dir == Common.Spellbound then [] else pure $ Common.formatDirection dir
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
