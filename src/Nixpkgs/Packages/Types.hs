{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Nixpkgs.Packages.Types
  ( PackageName(..)
  , displayPackageName
  , parsePackageName
  , PackageVersion(..)
  , displayPackageVersion
  , parsePackageVersion
  , ParsedPackageVersion
  , mkParsedPackageVersion
  )
where

import           Protolude

import           Data.Aeson
import           Data.Char
import           Data.String                    ( IsString(..) )
import qualified Data.Text                     as Text
import           Data.Versions
import qualified Repology.Versions             as Repology

newtype PackageName = PackageName
  { unPackageName :: Text
  } deriving (Eq, Generic, Hashable, Ord, Show)

instance FromJSON PackageName where
  parseJSON (String s) = pure . PackageName $ s
  parseJSON _          = mzero

instance ToJSON PackageName where
  toJSON = String . unPackageName

instance IsString PackageName where
  fromString = PackageName . toS

displayPackageName :: PackageName -> Text
displayPackageName (PackageName name) = name

-- | Helper function to extract package name. This follows the same semantics
-- as seen in nix -
-- https://github.com/NixOS/nix/blob/c94f3d5575d7af5403274d1e9e2f3c9d72989751/src/libexpr/names.cc#L14
parsePackageName :: Text -> PackageName
parsePackageName s =
  PackageName
    . Text.dropEnd
        ((Text.length . unPackageVersion . parsePackageVersion $ s) + 1)
    $ s

newtype PackageVersion = PackageVersion
  { unPackageVersion :: Text
  } deriving (Generic, Hashable, Show)

instance Eq PackageVersion where
  (PackageVersion v1) == (PackageVersion v2) =
    Repology.Version (toS v1) == Repology.Version (toS v2)

instance Ord PackageVersion where
  (PackageVersion v1) `compare` (PackageVersion v2) =
    Repology.Version (toS v1) `compare` Repology.Version (toS v2)

instance FromJSON PackageVersion where
  parseJSON (String s) = pure . PackageVersion $ s
  parseJSON _          = mzero

instance ToJSON PackageVersion where
  toJSON = String . unPackageVersion

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
  in  case Text.length suffix of
        0 -> PackageVersion suffix
        1 -> PackageVersion (suffix <> prefix)
        _ ->
          let c = Text.head . Text.drop 1 $ suffix
          in  if isDigit c
                then PackageVersion . Text.drop 1 $ suffix
                else parsePackageVersion . Text.drop 1 $ suffix

newtype ParsedPackageVersion =
  ParsedPackageVersion Versioning
  deriving (Eq, Ord, Show)

hush' :: Either e a -> Maybe a
hush' (Left  _) = Nothing
hush' (Right x) = Just x

mkParsedPackageVersion :: PackageVersion -> Maybe ParsedPackageVersion
mkParsedPackageVersion (PackageVersion v) =
  ParsedPackageVersion <$> (hush' . versioning $ v)
