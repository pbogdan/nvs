{-|
Module      : Nixpkgs.Packages
Description : Utilities to deal with package information extracted from nixpkgs.
Copyright   : (c) Piotr Bogdan, 2017
License     : BSD3
Maintainer  : ppbogdan@gmail.com
Stability   : experimental
Portability : Unknown

This module provides simple utilities to work with JSON representation of
nixpkgs packages.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Nixpkgs.Packages
  ( Package(..)
  , KeyedSet(..)
  , PackageSet
  , parsePackages
  )
where

import           Protolude               hiding ( packageName )

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Types
import qualified Data.ByteString               as Bytes
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import qualified Data.Set                      as Set
import           Data.String                    ( String )
import qualified Data.Vector                   as Vec
import           Nixpkgs.Packages.Types
import           Nvs.Types

-- | Main data type for representing package information.
data Package = Package
  { packageName :: PackageName -- ^ The name of the package
  , packageVersion :: PackageVersion -- ^ The version of the package
  } deriving (Eq, Generic, Show)

instance Ord Package where
  compare p1 p2 =
    let v1 = packageVersion p1
        v2 = packageVersion p2
        n1 = packageName p1
        n2 = packageName p2
    in  if v1 == v2 && n1 == n2 then EQ else v1 `compare` v2

instance FromJSON Package where
  parseJSON (Object o) =
    Package
      <$> (parsePackageName <$> o .: "name")
      <*> (parsePackageVersion <$> o .: "name")
  parseJSON _ = mzero

instance ToJSON Package where
  toJSON = genericToJSON $ aesonDrop (length ("Package" :: String)) camelCase

newtype KeyedSet a =
  KeyedSet (HashMap PackageName (Set a))
  deriving (Eq, Show)

instance Foldable KeyedSet where
  {-# INLINE foldr #-}
  foldr f z (KeyedSet t) = HashMap.foldr (\acc x -> Set.foldr f x acc) z t
  {-# INLINE foldl' #-}
  foldl' f z (KeyedSet t) = HashMap.foldl' (\acc x -> Set.foldl' f acc x) z t


instance (Semigroup (KeyedSet a), Ord a) => Monoid (KeyedSet a) where
  mempty = KeyedSet HashMap.empty
  (KeyedSet a) `mappend` (KeyedSet b) =
    KeyedSet (HashMap.unionWith Set.union a b)

type PackageSet = KeyedSet Package

parsePackage :: FromJSON a => Text -> Value -> Parser a
parsePackage attrPath (Object o) =
  let o' = HashMap.insert "attrPath" (String attrPath) o
  in  parseJSON (Object o')
parsePackage _ x = typeMismatch "ParsePackage" x

instance FromJSON (KeyedSet Package) where
  parseJSON (Object o) = do
    pkgs <- sequenceA . Vec.fromList . HashMap.elems $ HashMap.mapWithKey
      parsePackage
      o
    pure . KeyedSet . foldl' go HashMap.empty $ pkgs
   where
    go acc x =
      HashMap.insertWith Set.union (packageName x) (Set.singleton x) acc
  parseJSON x = typeMismatch "PackageSet" x

-- | Parse package informataion out of a JSON file. The expected format of the
-- JSON file is that produced by
--
-- > nix-env -qaP --json '*'
--
-- command. The result is a hash map keyed off packages' name.
parsePackages :: (MonadError NvsError m, MonadIO m) => FilePath -> m PackageSet
parsePackages path = do
  s <- liftIO . Bytes.readFile $ path
  let mRet = eitherDecodeStrict s
  either (throwError . FileParseError path . toS) return mRet
