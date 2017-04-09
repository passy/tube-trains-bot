{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module VersionInfo
  ( compilerVersionName
  , programVersion
  , formatGhcVersion
  ) where

import Protolude
import qualified Data.Digits as Digits
import qualified Data.Text as T
import qualified Data.Version as Version
import qualified Paths_tube_bot_fulfillment as Paths

versionNumber :: Maybe Int
#ifdef __GLASGOW_HASKELL__
versionNumber = Just __GLASGOW_HASKELL__
#else
versionNumber = Nothing
#endif

compilerVersionName :: Text
compilerVersionName =
  case formatGhcVersion =<< versionNumber of
    Just a -> "the Glorious Haskell Compiler version " <> a
    Nothing -> "an unknown Haskell Compiler"

-- | Turn the '__GLASGOW_HASKELL__' integer into something
-- users are used to reading.
formatGhcVersion :: Int -> Maybe Text
formatGhcVersion versionNum = do
  let digits = Digits.digits 10 versionNum
  (x:y:z:_) <- return digits
  return $ T.intercalate "." $ show <$> case y of
    0 -> [x, z]
    _ -> [x, y * 10 + z]

-- | Get the current Cabal version of this package.
programVersion :: Text
programVersion = T.pack . Version.showVersion $ Paths.version
