{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Api
  ( loadDeparturesForStation
  , Departure(..)
  , DepartureMap
  -- * For testing
  , parseDepartures
  ) where

import Protolude

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy   as BS
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import qualified Data.Text.Format as Format
import qualified Data.Vector as Vector
import qualified Network.Wreq as Wreq

import qualified Config
import qualified Common

import Control.Lens
import Data.Aeson.Lens

import Data.Aeson ((.:))

data Departure = Departure
  { departureLine :: Text -- TODO: Should probably be a proper type.
  , departureDestination :: Text
  , departureSeconds :: Int
  } deriving (Show, Eq)

instance Aeson.FromJSON Departure where
  parseJSON =
    Aeson.withObject "departure" $
      \o -> Departure <$> o .: "route_id"
                      <*> o .: "destination_name"
                      <*> o .: "time_seconds"

type DepartureMap = (HMS.HashMap Common.Direction [Departure])

stationUrl :: Format.Format
stationUrl =
  "https://citymapper.com/api/1/metrodepartures?headways=1&ids={}&region_id=uk-london"

mkUrlForStation :: Config.Config -> Maybe Text -> Text
mkUrlForStation Config.Config {Config.defaultStation} stationName =
  let station = fromMaybe (toStrict defaultStation) stationName
  in toStrict $ Format.format stationUrl (Format.Only station)

loadDeparturesForStation
  :: MonadIO m
  => Config.Config -> Maybe Text -> m (Maybe DepartureMap)
loadDeparturesForStation config stationName = do
  let url = mkUrlForStation config stationName
  r <- liftIO . Wreq.get $ T.unpack url
  return . parseDepartures $ r ^. Wreq.responseBody

parseDepartures
  :: BS.ByteString
  -> Maybe DepartureMap
parseDepartures r =
  -- TODO: This is super fragile. I should at iterate and find the right depature groupings. The first
  -- part appears to be fixed. I'm sure there's some cool Lens shit for this.
  let groupings :: Maybe (Vector.Vector Aeson.Value)
      groupings =
        r ^? key "stations"
           . nth 0
           . key "sections"
           . nth 0
           . key "departure_groupings"
           . _Array
      departures :: Maybe (Vector.Vector (Common.Direction, [Departure]))
      departures = sequence =<< fmap extractDepartures' <$> groupings

  in
    HMS.fromList . Vector.toList <$> departures

  where
    extractDepartures :: Aeson.Value -> Aeson.Parser (Common.Direction, [Departure])
    extractDepartures = Aeson.withObject "departure" $ \o -> (,) <$> o .: "direction_name" <*> o .: "departures"

    extractDepartures' :: Aeson.Value -> Maybe (Common.Direction, [Departure])
    extractDepartures' = Aeson.parseMaybe extractDepartures
