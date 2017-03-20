{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module VersionInfo
  ( compilerVersionName
  , programVersion
  ) where

import Protolude
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
  case versionNumber of
    Just a -> "the Glorious Haskell Compiler version " <> formatVersion a
    Nothing -> "an unknown Haskell Compiler"
  where
    formatVersion :: Int -> Text
    formatVersion = T.intersperse '.' . show

programVersion :: Text
programVersion = T.pack . Version.showVersion $ Paths.version
