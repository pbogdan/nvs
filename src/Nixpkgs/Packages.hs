{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Nixpkgs.Packages
  ( Package(..)
  , KeyedSet(..)
  , PackageSet
  )
where

import           Protolude               hiding ( packageName )

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import qualified Data.Set                      as Set
import           Data.String                    ( String )
import           Nixpkgs.Packages.Types

data Package a = Package
  { packageName :: PackageName
  , packageVersion :: PackageVersion
  , packagePatches :: [a]
  } deriving (Eq, Generic, Show)

instance Ord a => Ord (Package a) where
  compare p1 p2 =
    let v1 = packageVersion p1
        v2 = packageVersion p2
        n1 = packageName p1
        n2 = packageName p2
    in  if v1 == v2 && n1 == n2 then EQ else v1 `compare` v2

instance FromJSON (Package a) where
  parseJSON (Object o) =
    Package
      <$> (parsePackageName <$> o .: "name")
      <*> (parsePackageVersion <$> o .: "name")
      <*> pure []
  parseJSON _ = mzero

instance ToJSON a => ToJSON (Package a) where
  toJSON = genericToJSON $ aesonDrop (length ("Package" :: String)) camelCase

newtype KeyedSet a =
  KeyedSet (HashMap PackageName (Set a))
  deriving (Eq, Show)

instance Foldable KeyedSet where
  {-# INLINE foldr #-}
  foldr f z (KeyedSet t) = HashMap.foldr (flip (Set.foldr f)) z t
  {-# INLINE foldl' #-}
  foldl' f z (KeyedSet t) = HashMap.foldl' (Set.foldl' f) z t

instance (Semigroup (KeyedSet a), Ord a) => Monoid (KeyedSet a) where
  mempty = KeyedSet HashMap.empty
  (KeyedSet a) `mappend` (KeyedSet b) =
    KeyedSet (HashMap.unionWith Set.union a b)

type PackageSet a = KeyedSet (Package a)
