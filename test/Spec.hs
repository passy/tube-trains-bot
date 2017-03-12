{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import Protolude
import System.Directory (getCurrentDirectory )
import System.FilePath ((</>))

import qualified Data.Aeson             as Aeson
import qualified Data.ByteString.Lazy   as BS
import qualified Data.Text.Encoding     as TE
import qualified Data.Text.IO           as TIO

import qualified Api

import Test.Hspec

readFixture :: FilePath -> IO BS.ByteString
readFixture path = do
    dir <- getCurrentDirectory
    BS.fromStrict . TE.encodeUtf8 <$> TIO.readFile (dir </> "test" </> "fixtures" </> path)

main :: IO ()
main = hspec $ do
  describe "Tube Bot" $ do
    describe "JSON Parsing" $ do

      it "parses a search response" $ do
        resp <- readFixture "metrodepartures.json"
        Api.parseDepartures resp `shouldSatisfy` isJust
