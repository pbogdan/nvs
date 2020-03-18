{-|
Module      : Nvs.Aliases
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

The database is currently stored in YAML format in @data/package-aliases.yaml@
file. The format of the database is as follows:

@
  - package: nixpkgs-package-name
  - aliases:
    - alternative-name-1
    - alternative-name-2
@

-}

{-# LANGUAGE FlexibleContexts #-}

module Nixpkgs.Packages.Aliases
  ( PackageAlias(..)
  , parseAliases
  )
where

import           Protolude

import qualified Data.ByteString               as Bytes
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import qualified Data.Vector                   as Vec
import           Data.Yaml
import           Nixpkgs.Packages.Types
import           Nvs.Types

-- | Representation of a single entry from aliases database.
data PackageAlias = PackageAlias
  { packageAliasPackage :: PackageName -- ^ name of the package in nixpkgs
                                       -- package -- collection
  , packageAliasAliases :: [PackageName] -- ^ list of alternative names of the
                                         -- nixpkgs -- package
  } deriving (Eq, Show)

instance FromJSON PackageAlias where
  parseJSON (Object o) = PackageAlias <$> o .: "package" <*> o .: "aliases"
  parseJSON _          = mzero

-- | Load and parse aliases database. The returned hash map is keyed off of the
-- names of nixpkgs packages.
parseAliases
  :: (MonadError NvsError m, MonadIO m)
  => FilePath -- ^ path to aliases database file
  -> m (HashMap PackageName PackageAlias)
parseAliases path = do
  s <- liftIO . Bytes.readFile $ path
  let parser = withArray "Aliases" $ \a -> sequenceA (parseJSON <$> a)
  let mRet   = join (parseEither parser <$> decodeEither s)
  case mRet of
    Left  e   -> throwError . FileParseError path . toS $ e
    Right ret -> return . Vec.foldl' go HashMap.empty $ ret
 where
  go
    :: HashMap PackageName PackageAlias
    -> PackageAlias
    -> HashMap PackageName PackageAlias
  go acc x = HashMap.insert (packageAliasPackage x) x acc
