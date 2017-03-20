{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Protolude
import System.Directory (getCurrentDirectory )
import System.FilePath ((</>))
import Data.String (String)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO

import qualified Api

import Test.Hspec

readFixture :: FilePath -> IO BS.ByteString
readFixture path = do
    dir <- getCurrentDirectory
    BS.fromStrict . TE.encodeUtf8 <$> TIO.readFile (dir </> "test" </> "fixtures" </> path)

superUnsafeParse :: Aeson.FromJSON a => BS.ByteString -> a
superUnsafeParse = either (error . T.pack) identity . Aeson.eitherDecode

main :: IO ()
main = hspec $ do
  describe "Tube Bot" $ do
    describe "JSON Parsing" $ do

      it "parses a search response" $ do
        resp <- readFixture "metrodepartures.json"
        Api.parseDepartures resp `shouldSatisfy` isJust

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
