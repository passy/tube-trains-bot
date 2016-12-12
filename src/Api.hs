{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Api () where

import Protolude hiding ((<>))
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.Format as Format
import qualified Network.Wreq as Wreq
import qualified Data.Vector as Vector

import qualified Config

import Control.Lens
import Data.Aeson.Lens

data Direction = Westbound | Eastbound | Northbound | Southbound | Spellbound
  deriving (Show, Eq)

-- FIXME
data Departure = Departure

instance Aeson.FromJSON Direction where
  parseJSON (Aeson.String a) | a == "Westbound" = return Westbound
                             | a == "Eastbound" = return Eastbound
                             | a == "Northbound" = return Northbound
                             | a == "Southbound" = return Northbound
                             | otherwise = return Spellbound

stationUrl :: Format.Format
stationUrl = "https://citymapper.com/api/1/metrodepartures?headways=1&ids={}&region_id=uk-london"

mkUrlForStation :: Config.Config -> Maybe Text -> Text
mkUrlForStation Config.Config{Config.defaultStation} stationName =
  let station = fromMaybe (toStrict defaultStation) stationName
  in toStrict $ Format.format stationUrl (Format.Only station)

(<$$>) :: (Functor f, Functor g) => (a -> b) -> g (f a) -> g (f b)
(<$$>) = fmap . fmap

infixr 8 <$$>

getDeparturesForStation :: MonadIO m => Config.Config -> Maybe Text -> m (Maybe (Vector.Vector (Direction, [Departure])))
getDeparturesForStation config stationName = do
  let url = mkUrlForStation config stationName
  r <- liftIO . Wreq.get $ T.unpack url
  -- TODO: This is super fragile. I should at iterate and find the right depature groupings. The first
  -- part appears to be fixed. I'm sure there's some cool Lens shit for this.
  let groupings :: Maybe (Vector.Vector Aeson.Value)
      groupings = r ^? Wreq.responseBody . key "stations" . nth 0 . key "sections" . nth 0 . key "departure_groupings" . _Array
      mdepartures :: Maybe (Vector.Vector (Maybe (Direction, [Departure])))
      mdepartures = (\vec -> extractDepartures <$> vec) <$> groupings
      departures :: Maybe (Vector.Vector (Direction, [Departure]))
      departures = wat mdepartures

  return departures

  where
    wat :: Maybe (Vector.Vector (Maybe (Direction, [Departure]))) -> Maybe (Vector.Vector (Direction, [Departure]))
    wat mvec = do
      vec <- mvec
      wat <- sequence vec
      return wat

    extractDepartures :: Aeson.Value -> Maybe (Direction, [Departure])
    extractDepartures a = do
      direction <- a ^? key "direction_name"
      departures <- a ^? key "departures"
      -- Fixme
      return (Westbound, [Departure])
