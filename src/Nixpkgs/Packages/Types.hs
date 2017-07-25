{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Nixpkgs.Packages.Types where

import Protolude

import Data.Aeson
import Data.Hashable

newtype PackageName = PackageName
  { unPackageName :: Text
  } deriving (Eq, Generic, Hashable, Ord, Show)

instance ToJSON PackageName where
  toJSON (PackageName n) = String n

newtype PackageVersion = PackageVersion
  { unPackageVersion :: Text
  } deriving (Eq, Generic, Ord, Show)

instance ToJSON PackageVersion where
  toJSON (PackageVersion v) = String v
