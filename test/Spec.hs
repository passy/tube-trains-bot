{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Protolude
import System.Directory (getCurrentDirectory )
import System.FilePath ((</>))
import Data.String (String)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BS
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
superUnsafeParse = either (error . T.pack) identity . Aeson.eitherDecode

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

      it "doesn't regress on london bridge" $ do
        fixt <- readFixture "lbridge_departures.json"
        Just res <- return $ Api.parseDepartures fixt
        let resp = do
              Response.departures res
              Response.station $ Common.StationName "LondonBridge"
              Response.direction $ Common.Spellbound

        let resp' = Response.runResponse (Response.mkCoResponse testConfig) resp
        Common._speech resp' `shouldNotBe` "I could not find any departures for the given parameters."


      it "parses a departure" $ do
        resp <- readFixture "singledeparture.json"
        let dp :: Api.Departure
            dp = superUnsafeParse resp

        Api.departureLine dp `shouldBe` "District"
        Api.departureDestination dp `shouldBe` "West Ham"
        Api.departureSeconds dp `shouldBe` 187

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
