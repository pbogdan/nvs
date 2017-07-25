{-|
Module      : Nixpkgs.Vuln.Aliases
Description : Package aliases support.
Copyright   : (c) Piotr Bogdan, 2017
License     : BSD3
Maintainer  : ppbogdan@gmail.com
Stability   : experimental
Portability : Unknown

Package aliases support.

-}

module Nixpkgs.Packages.Aliases
  ( PackageAlias(..)
  , parseAliases
  ) where

import           Protolude

import qualified Data.ByteString as Bytes
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vec
import           Data.Yaml

data PackageAlias = PackageAlias
  { packageAliasPackage :: Text
  , packageAliasAliases :: [Text]
  } deriving (Eq, Show)

instance FromJSON PackageAlias where
  parseJSON (Object o) = PackageAlias <$> o .: "package" <*> o .: "aliases"
  parseJSON _ = mzero

parseAliases :: FilePath -> IO (HashMap Text PackageAlias)
parseAliases path = do
  s <- Bytes.readFile path
  let parser = withArray "Aliases" $ \a -> sequenceA (parseJSON <$> a)
  let mRet = join (parseEither parser <$> decodeEither s)
  case mRet of
    Left e -> panic . toS $ e
    Right ret -> return . Vec.foldl' go HashMap.empty $ ret
  where
    go :: HashMap Text PackageAlias -> PackageAlias -> HashMap Text PackageAlias
    go acc x = HashMap.insert (packageAliasPackage x) x acc
