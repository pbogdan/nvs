{-|
Module      : Nixpkgs.Vuln.Aliases
Description : Package aliases support.
Copyright   : (c) Piotr Bogdan, 2017
License     : BSD3
Maintainer  : ppbogdan@gmail.com
Stability   : experimental
Portability : Unknown

Alias and alias database functions and types. Aliases are required in cases
where the name of a package in nipxkgs collection is different to what's present
in a source of vulnerabilities. For example @vlc@ from nixpkgs is present in NVD
under the name of @vlc_media_player@.

The database is currently stored in YAML format in @data/aliases.yaml@ file. The
format of the database is as follows:

@
  - package: nixpkgs-package-name
  - aliases:
    - alternative-name-1
    - alternative-name-2
@

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

-- | Representation of a single entry from aliases database.
data PackageAlias = PackageAlias
  { packageAliasPackage :: Text -- ^ name of the package in nixpkgs package
                                -- collection
  , packageAliasAliases :: [Text] -- ^ list of alternative names of the nixpkgs
                                  -- package
  } deriving (Eq, Show)

instance FromJSON PackageAlias where
  parseJSON (Object o) = PackageAlias <$> o .: "package" <*> o .: "aliases"
  parseJSON _ = mzero

-- | Load and parse aliases database. The returned hash map is keyed off of the
-- names of nixpkgs packages.
parseAliases ::
     FilePath -- ^ path to aliases database file
  -> IO (HashMap Text PackageAlias)
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
