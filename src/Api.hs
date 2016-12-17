{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Api
  ( loadDeparturesForStation
  , Departure(..)
  , DepartureMap(..)
  ) where

import Protolude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as T
import qualified Data.Text.Format as Format
import qualified Network.Wreq as Wreq
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HMS

import qualified Config
import qualified Common
import qualified Debug.Trace as Trace

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
    \o ->
       Departure <$> o .: "route_id" <*> o .: "destination_name" <*> o .:
       "time_seconds"

newtype DepartureMap =
  DepartureMap (HMS.HashMap Common.Direction [Departure])
  deriving (Show, Eq)

-- | Like `fromMaybe` but providing the default value in the case of an
-- empty argument too.
fromMayEmpty :: Text -> Maybe Text -> Text
fromMayEmpty d Nothing = d
fromMayEmpty d (Just t) | T.null t = d
                        | otherwise = t

stationUrl :: Format.Format
stationUrl =
  "https://citymapper.com/api/1/metrodepartures?headways=1&ids={}&region_id=uk-london"

mkUrlForStation :: Config.Config -> Maybe Text -> Text
mkUrlForStation Config.Config {Config.defaultStation} stationName =
  let station = fromMayEmpty (toStrict defaultStation) stationName
  in toStrict $ Format.format stationUrl (Format.Only station)

loadDeparturesForStation
  :: MonadIO m
  => Config.Config -> Maybe Text -> m (Maybe DepartureMap)
loadDeparturesForStation config stationName = do
  let url = mkUrlForStation config stationName
  r <- liftIO $ Wreq.get (Trace.traceId $ T.unpack url)
  -- TODO: This is super fragile. I should at iterate and find the right depature groupings. The first
  -- part appears to be fixed. I'm sure there's some cool Lens shit for this.
  let groupings :: Maybe (Vector.Vector Aeson.Value)
      groupings =
        r ^? Wreq.responseBody
           . key "stations"
           . nth 0
           . key "sections"
           . nth 0
           . key "departure_groupings"
           . _Array
      departures :: Maybe (Vector.Vector (Common.Direction, [Departure]))
      departures = sequence =<< fmap extractDepartures' <$> groupings
  return $ DepartureMap . HMS.fromList . Vector.toList <$> departures
  where
    extractDepartures :: Aeson.Value -> Aeson.Parser (Common.Direction, [Departure])
    extractDepartures = Aeson.withObject "departure" $ \o -> (,) <$> o .: "direction_name" <*> o .: "departures"
    extractDepartures' :: Aeson.Value -> Maybe (Common.Direction, [Departure])
    extractDepartures' = Aeson.parseMaybe extractDepartures
