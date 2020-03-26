{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Nixpkgs.Packages
  ( Package(..)
  )
where

import           Protolude               hiding ( packageName )

import           Data.Aeson
import           Data.Aeson.Casing
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
