{-|
Module      : Nixpkgs.Packages.Types
Description : Common package related types and utilities.
Copyright   : (c) Piotr Bogdan, 2017
License     : BSD3
Maintainer  : ppbogdan@gmail.com
Stability   : experimental
Portability : Unknown

Common package related types and utilities.

-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Nixpkgs.Packages.Types
  ( PackageName
  , displayPackageName
  , parsePackageName
  , PackageVersion
  , displayPackageVersion
  , parsePackageVersion
  , wildcard
  ) where

import           Protolude

import           Data.Aeson
import           Data.Char
import           Data.Hashable
import           Data.String (IsString(..))
import qualified Data.Text as Text

newtype PackageName =
  PackageName Text
  deriving (Eq, Generic, Hashable, Ord, Show)

instance FromJSON PackageName where
  parseJSON (String s) = pure . PackageName $ s
  parseJSON _ = mzero

instance ToJSON PackageName where
  toJSON (PackageName n) = String n

instance IsString PackageName where
  fromString = PackageName . toS

displayPackageName :: PackageName -> Text
displayPackageName (PackageName name) = name

-- | Helper function to extract package name. This follows the same semantics
-- as seen in nix -
-- https://github.com/NixOS/nix/blob/c94f3d5575d7af5403274d1e9e2f3c9d72989751/src/libexpr/names.cc#L14
parsePackageName :: Text -> PackageName
parsePackageName s =
  PackageName .
  Text.dropEnd ((Text.length . unPackageVersion . parsePackageVersion $ s) + 1) $
  s

newtype PackageVersion = PackageVersion
  { unPackageVersion :: Text
  } deriving (Eq, Generic, Hashable, Ord, Show)

instance FromJSON PackageVersion where
  parseJSON (String s) = pure . PackageVersion $ s
  parseJSON _ = mzero

instance ToJSON PackageVersion where
  toJSON (PackageVersion v) = String v

instance IsString PackageVersion where
  fromString = PackageVersion . toS

displayPackageVersion :: PackageVersion -> Text
displayPackageVersion (PackageVersion ver) = ver

-- | Helper function to extract package version. This follows the same semantics
-- as seen in nix -
-- https://github.com/NixOS/nix/blob/c94f3d5575d7af5403274d1e9e2f3c9d72989751/src/libexpr/names.cc#L14
parsePackageVersion :: Text -> PackageVersion
parsePackageVersion s =
  let prefix = Text.takeWhile (/= '-') s
      suffix = Text.drop (Text.length prefix) s
  in case Text.length suffix of
       0 -> PackageVersion suffix
       1 -> PackageVersion (suffix <> prefix)
       _ ->
         let c = Text.head . Text.drop 1 $ suffix
         in if isDigit c
              then PackageVersion . Text.drop 1 $ suffix
              else parsePackageVersion . Text.drop 1 $ suffix

wildcard :: PackageVersion
wildcard = PackageVersion "*"
