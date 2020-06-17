{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Response where

import Protolude hiding ((<>), (&))
import Data.Monoid ((<>))

-- Operator imports
import Control.Lens (at)
import Control.Lens.Iso (non)
import Control.Lens.Operators ((<>~), (?~), (.~), (&))
import Control.Lens.TH (makeLenses)
import Data.Pairing (Pairing(pair))

import qualified Control.Monad.Free as Free
import qualified Control.Comonad.Cofree as Cofree
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T

import qualified Config
import qualified Common
import qualified Api

-- * Fulfillment response combinators

data ResponseF r =
    AbortF Common.FulfillmentError r
  | LineF Common.LineName r
  | StationF Common.StationName r
  | DirectionF Common.Direction r
  | DepartureF Common.Direction Api.Departure r
  | DeparturesF Api.DepartureMap r
  deriving (Show, Functor)

type Response = Free.Free ResponseF

abort
  :: Common.FulfillmentError
  -> Response ()
abort err = Free.liftF $ AbortF err ()

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
  :: Common.LineName
  -> Response ()
line txt = Free.liftF $ LineF txt ()

station
  :: Common.StationName
  -> Response ()
station txt = Free.liftF $ StationF txt ()

data ResponseState = ResponseState
  { _sError :: Maybe Common.FulfillmentError
  , _sStation :: Maybe Common.StationName
  , _sLine :: Maybe Common.LineName
  , _sDepartures :: Api.DepartureMap
  , _sDirection :: Common.Direction
  , _sConfig :: Config.Config }
  deriving (Show)

makeLenses ''ResponseState

mkResponseState :: Config.Config -> ResponseState
mkResponseState = ResponseState empty empty empty mempty Common.Spellbound

runResponse
  :: CoResponse ResponseState
  -> Response ()
  -> Common.WebhookFulfillment
runResponse coresp resp = format $ pair const coresp resp
  where
    format ResponseState{..} =
      case _sError of
        Just (Common.FulfillmentError err) -> Common.mkFulfillment err
        Nothing -> filterDepartures _sConfig _sDirection _sStation _sLine _sDepartures

-- * Comonad for the Response DSL

data CoResponseF k = CoResponseF
  { abortH :: Common.FulfillmentError -> k
  , lineH :: Common.LineName -> k
  , stationH :: Common.StationName -> k
  , directionH :: Common.Direction -> k
  , departureH :: Common.Direction -> Api.Departure -> k
  , departuresH :: Api.DepartureMap -> k }
  deriving Functor

type CoResponse = Cofree.Cofree CoResponseF

-- * Comonadic machinery for CoResponse

mkCoResponse :: Config.Config -> CoResponse ResponseState
mkCoResponse c = Cofree.coiter next start'
  where
    start' :: ResponseState
    start' = mkResponseState c
    next w = CoResponseF
      (coAbort w)
      (coLine w)
      (coStation w)
      (coDirection w)
      (coDeparture w)
      (coDepartures w)

coAbort :: ResponseState -> Common.FulfillmentError -> ResponseState
coAbort s err = s & sError ?~ err

coLine :: ResponseState -> Common.LineName -> ResponseState
coLine s line' = s & sLine ?~ line'

coStation :: ResponseState -> Common.StationName -> ResponseState
coStation s station' = s & sStation ?~ station'

coDirection :: ResponseState -> Common.Direction -> ResponseState
coDirection s direction' = s & sDirection .~ direction'

coDeparture :: ResponseState -> Common.Direction -> Api.Departure -> ResponseState
coDeparture s dir departure' =
  -- This warrants a bit of documentation:
  -- `sDepartures` is the lens obviously, `at dir` works on the hashmap but gives
  -- us a Maybe. Using a _Just prism would work, but then we wouldn't append if it was
  -- Nothing, so we want to supply a default value, which is exactly what the `non` iso
  -- does. `<>~` `mappend`s a new value and even sets it to Just if neccessary.
  -- Cool? Cool.
  s & sDepartures . at dir . non [] <>~ pure departure'

coDepartures :: ResponseState -> Api.DepartureMap -> ResponseState
coDepartures s dm = s & sDepartures .~ dm

-- * Pairing between Free and Cofree

instance Pairing CoResponseF ResponseF where
  pair f CoResponseF{..} (AbortF err k) = f (abortH err) k
  pair f CoResponseF{..} (LineF v k) = f (lineH v) k
  pair f CoResponseF{..} (StationF v k) = f (stationH v) k
  pair f CoResponseF{..} (DirectionF v k) = f (directionH v) k
  pair f CoResponseF{..} (DepartureF dir dep k) = f (departureH dir dep) k
  pair f CoResponseF{..} (DeparturesF dm k) = f (departuresH dm) k

instance Pairing ResponseF CoResponseF where
  pair f (AbortF err k) CoResponseF{..} = f k (abortH err)
  pair f (LineF v k) CoResponseF{..} = f k (lineH v)
  pair f (StationF v k) CoResponseF{..} = f k (stationH v)
  pair f (DirectionF v k) CoResponseF{..} = f k (directionH v)
  pair f (DepartureF dir dep k) CoResponseF{..} = f k (departureH dir dep)
  pair f (DeparturesF dm k) CoResponseF{..} = f k (departuresH dm)

-- * Helpers to make the responses work

filterLine
  :: Common.LineName
  -> [Api.Departure]
  -> Maybe [Api.Departure]
filterLine (Common.LineName l) ds =
  let res = filter (\d -> Api.departureLine d == l) ds
  in if null res
        then Nothing
        else Just res

filterDepartures
  :: Config.Config
  -> Common.Direction
  -> Maybe Common.StationName
  -> Maybe Common.LineName
  -> Api.DepartureMap
  -> Common.WebhookFulfillment
filterDepartures c dir station' mline d =
  -- Filter by line if filter is provided
  let d' = HMS.mapMaybe (maybe pure filterLine mline) d
  in
    Common.mkFulfillment $ case (null d', station', HMS.lookup dir d') of
      (True, _, _) -> "I could not find any live departures for this station."
      -- There was no station specified
      (False, Nothing, _) -> "I'm not sure which station to look for."
      -- We found departures for the specified direction.
      (False, Just s, Just ds) -> formatDepartures c dir s ds
      -- We can't filter by direction, so we'll list them all.
      (False, Just s, Nothing) ->
        let go m k v = formatDepartures c k s v : m
            l = HMS.foldlWithKey' go empty d'
        in T.unwords l

-- * Formatters to make things understandable to the user.

-- TODO: Refactor this. The arity is WAY TOO HIGH!
formatDepartures :: Config.Config -> Common.Direction -> Common.StationName -> [Api.Departure] -> Text
formatDepartures _ Common.Spellbound _ [] =
  "Sorry, there don't seem to be any departures from this station at the moment."
formatDepartures _ dir _ [] =
     "Sorry, there don't seem to be any "
  <> Common.formatDirection dir
  <> " departures from this station at the moment."
formatDepartures c dir (Common.StationName station') ds =
  let directionTxt :: [Text]
      directionTxt = if dir == Common.Spellbound then [] else pure $ Common.formatDirection dir
      enquote :: Text -> Text
      enquote t = "\"" <> t <> "\""
      preamble :: [Text]
      preamble = [ "I found the following" ]
                 ++ directionTxt
                 ++ [ "departures from "
                    , unCamelCase station' <> ":" ]
      format d = unCamelCase (Api.departureLine d)
              <> " line to "
              <> enquote (Api.departureDestination d)
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
