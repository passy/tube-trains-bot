{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Protolude
import System.Directory (getCurrentDirectory )
import System.FilePath ((</>))
import Data.String (String)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO

import qualified Api
import qualified VersionInfo
import qualified Common
import qualified Response
import qualified Config

import Test.Hspec

readFixture :: FilePath -> IO BS.ByteString
readFixture path = do
    dir <- getCurrentDirectory
    BS.fromStrict . TE.encodeUtf8 <$> TIO.readFile (dir </> "test" </> "fixtures" </> path)

superUnsafeParse :: Aeson.FromJSON a => BS.ByteString -> a
superUnsafeParse = either (panic . T.pack) identity . Aeson.eitherDecode

testConfig :: Config.Config
testConfig =
  Config.Config { Config.port = 1024
                , Config.maxDeparturesPerDirection = 3
                }

main :: IO ()
main = hspec $ do
  describe "Tube Bot" $ do
    describe "JSON Parsing" $ do

      it "parses a search response" $ do
        resp <- readFixture "metrodepartures.json"
        Api.parseDepartures resp `shouldSatisfy` isJust

      it "handle time-less responses" $ do
        fixt <- readFixture "lbridge_departures_notimes.json"
        Just res <- return $ Api.parseDepartures fixt
        res `shouldBe` HMS.empty

      it "provides a full response" $ do
        fixt <- readFixture "lbridge_departures.json"
        Just res <- return $ Api.parseDepartures fixt
        let resp = do
              Response.departures res
              Response.station $ Common.StationName "LondonBridge"
              Response.direction $ Common.Spellbound

        let resp' = Response.runResponse (Response.mkCoResponse testConfig) resp
        Common._speech resp' `shouldBe` "I found the following Eastbound departures from  London Bridge: Jubilee line to \"Stratford\" in less than a minute. Jubilee line to \"West Ham\" in 2 minutes. Jubilee line to \"Stratford\" in 4 minutes. I found the following Westbound departures from  London Bridge: Jubilee line to \"Stanmore\" in 1 minutes. Jubilee line to \"Stanmore\" in 3 minutes. Jubilee line to \"Willesden Green\" in 5 minutes."

      it "provides a full response for aldgate east" $ do
        fixt <- readFixture "aldgateeast_departures.json"
        Just res <- return $ Api.parseDepartures fixt
        length res `shouldBe` 5
        let resp = do
              Response.departures res
              Response.station $ Common.StationName "AldgateEast"
              Response.direction $ Common.Spellbound

        let resp' = Response.runResponse (Response.mkCoResponse testConfig) resp
        Common._speech resp' `shouldBe` "I found the following Eastbound departures from  London Bridge: Jubilee line to \"Stratford\" in less than a minute. Jubilee line to \"West Ham\" in 2 minutes. Jubilee line to \"Stratford\" in 4 minutes. I found the following Westbound departures from  London Bridge: Jubilee line to \"Stanmore\" in 1 minutes. Jubilee line to \"Stanmore\" in 3 minutes. Jubilee line to \"Willesden Green\" in 5 minutes."

      it "parses a departure" $ do
        resp <- readFixture "singledeparture.json"
        let dp :: Api.Departure
            dp = superUnsafeParse resp

        Api.departureLine dp `shouldBe` "District"
        Api.departureDestination dp `shouldBe` "West Ham"
        Api.departureSeconds dp `shouldBe` 187

      it "parses a single departure to hammersmith" $ do
        resp <- readFixture "singledeparture_hammersmith.json"
        let dp :: Api.Departure
            dp = superUnsafeParse resp

        Api.departureLine dp `shouldBe` "HammersmithAndCity"
        Api.departureDestination dp `shouldBe` "Hammersmith"
        Api.departureSeconds dp `shouldBe` 390

      it "fails to parse a departure without seconds" $ do
        resp <- readFixture "singledeparture_noseconds.json"
        let dp :: Either String Api.Departure
            dp = Aeson.eitherDecode resp

        dp `shouldSatisfy` isLeft

    describe "Version Info" $ do

      it "formats major version" $ do
        VersionInfo.formatGhcVersion 800 `shouldBe` Just "8.0"

      it "formats minor version" $ do
        VersionInfo.formatGhcVersion 801 `shouldBe` Just "8.1"

      it "formats double-decimal version" $ do
        VersionInfo.formatGhcVersion 820 `shouldBe` Just "8.20"

      it "formats trailing zero" $ do
        VersionInfo.formatGhcVersion 710 `shouldBe` Just "7.10"
