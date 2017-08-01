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

module Nixpkgs.Packages
  ( Package(..)
  , packageUrl
  , PackageMeta(..)
  , PackageLicense(..)
  , LicenseDetails(..)
  , parsePackages
  ) where

import           Protolude

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Types
import qualified Data.ByteString as Bytes
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.String (String)
import qualified Data.Text as Text
import qualified Data.Vector as Vec
import           Nixpkgs.Packages.Types
import           Nvs.Types

-- | Main data type for representing package information.
data Package = Package
  { packageSystem :: Text -- ^ The system on which the information has been
                          -- extracted
  , packageName :: PackageName -- ^ The name of the package
  , packageVersion :: PackageVersion -- ^ The version of the package
  , packageMeta :: PackageMeta -- ^ The meta data of the package
  } deriving (Eq, Generic, Show)

instance FromJSON Package where
  parseJSON (Object o) =
    Package <$> o .: "system" <*> (parsePackageName <$> o .: "name") <*>
    (parsePackageVersion <$> o .: "name") <*>
    o .: "meta"
  parseJSON _ = mzero

instance ToJSON Package where
  toJSON = genericToJSON $ aesonDrop (length ("Package" :: String)) camelCase

-- | Package meta data.
data PackageMeta = PackageMeta
  { packageMetaPlatforms :: Maybe [Text] -- ^ list platforms on which the
                                         -- package is supported
  , packageMetaMaintainers :: Maybe [Text] -- ^ list of package maintainers
  , packageMetaDescription :: Maybe Text -- ^ package description
  , packageMetaLicense :: Maybe [PackageLicense] -- ^ licenses of the package
  , packageMetaPosition :: Maybe Text -- ^ source position of where the package
                                      -- is defined within nixpkgs
  , packageMetaHomepage :: Maybe [Text] -- ^ package homepage
  , packageMetaLongDescription :: Maybe Text -- ^ long description
  } deriving (Eq, Generic, Show)

instance FromJSON PackageMeta where
  parseJSON (Object o) =
    PackageMeta <$> o .:? "platforms" <*>
    (o .:? "maintainers" <|> (sequenceA . singleton <$> o .:? "maintainers")) <*>
    o .:? "description" <*>
    (o .:? "license" <|> (sequenceA . singleton <$> o .:? "license")) <*>
    o .:? "position" <*>
    (o .:? "homepage" <|> (sequenceA . singleton <$> o .:? "homepage")) <*>
    o .:? "longDescription"
    where
      singleton :: a -> [a]
      singleton x = [x]
  parseJSON _ = mzero

instance ToJSON PackageMeta where
  toJSON =
    genericToJSON $ aesonDrop (length ("PackageMeta" :: String)) camelCase

-- | Data type representing license of a package. The license may be a simple
-- string, such as "gpl2", represented with the 'BasicLicence' constructor, or
-- may take more detailed form of 'DetailedLicense'.
data PackageLicense
  = DetailedLicense LicenseDetails
  | BasicLicense Text
  deriving (Eq, Generic, Show)

instance ToJSON PackageLicense where
  toJSON =
    genericToJSON $ aesonDrop (length ("PackageLicense" :: String)) camelCase

instance FromJSON PackageLicense where
  parseJSON js@(Object _) = DetailedLicense <$> parseJSON js
  parseJSON (String s) = pure . BasicLicense $ s
  parseJSON _ = mzero

-- | Detailed representation of a license.
data LicenseDetails = LicenseDetails
  { detailedLicenseShortName :: Maybe Text -- ^ short name such as "gpl2"
  , detailedLicenseFullName :: Maybe Text -- ^ full name such as "GNU General
                                          -- Public License v2.0"
  , detailedLicenseUrl :: Maybe Text -- ^ URL at which the license may be
                                     -- obtained
  , detailedLicenseSpdxId :: Maybe Text -- ^ license id within SPDX license list
                                        -- - https://spdx.org/licenses/
  } deriving (Eq, Generic, Show)

instance FromJSON LicenseDetails where
  parseJSON (Object o) =
    LicenseDetails <$> o .:? "shortName" <*> o .:? "fullName" <*> o .:? "url" <*>
    o .:? "spdxId"
  parseJSON _ = mzero

instance ToJSON LicenseDetails where
  toJSON =
    genericToJSON $ aesonDrop (length ("LicenseDetails" :: String)) camelCase

-- | Parse package informataion out of a JSON file. The expected format of the
-- JSON file is that produced by
--
-- > nix-env -qaP --json '*'
--
-- command. The result is a hash map keyed off packages' name.
parsePackages ::
     (MonadError NvsError m, MonadIO m)
  => FilePath
  -> m (HashMap PackageName Package)
parsePackages path = do
  s <- liftIO . Bytes.readFile $ path
  let parser =
        withObject "Packages" $ \o ->
          sequenceA (parseJSON <$> (Vec.fromList . HashMap.elems $ o))
  let mRet = join (parseEither parser <$> eitherDecodeStrict s)
  case mRet of
    Left e -> throwError . FileParseError path . toS $ e
    Right ret -> return . Vec.foldl' go HashMap.empty $ ret
  where
    go :: HashMap PackageName Package -> Package -> HashMap PackageName Package
    go acc x = HashMap.insert (packageName x) x acc

-- | GitHub link to the file containing Nix expression that defined the
-- package. The information required to build the link may not be present in
-- package's meta data in which case 'Nothing' is returned.
packageUrl :: Package -> Maybe Text
packageUrl pkg =
  let position = packageMetaPosition . packageMeta $ pkg
      path = fst . Text.breakOn ":" . snd . Text.breakOn "/pkgs" <$> position
  in ("https://github.com/NixOS/nixpkgs/blob/release-17.03/" <>) <$> path
