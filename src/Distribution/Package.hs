{-# LANGUAGE DeriveGeneric #-}

module Distribution.Package
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
import           Data.Char
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.String (String)
import qualified Data.Text as Text
import qualified Data.Vector as Vec

data Package = Package
  { packageSystem :: Text
  , packageName :: Text
  , packageVersion :: Text
  , packageMeta :: PackageMeta
  } deriving (Eq, Generic, Show)

instance FromJSON Package where
  parseJSON (Object o) =
    Package <$> o .: "system" <*> (parseName <$> o .: "name") <*>
    (parseVersion <$> o .: "name") <*>
    o .: "meta"
  parseJSON _ = mzero

instance ToJSON Package where
  toJSON = genericToJSON $ aesonDrop (length ("Package" :: String)) camelCase

parseVersion :: Text -> Text
parseVersion s =
  let prefix = Text.takeWhile (/= '-') s
      suffix = Text.drop (Text.length prefix) s
  in case Text.length suffix of
       0 -> suffix
       1 -> suffix <> prefix
       _ ->
         let c = Text.head . Text.drop 1 $ suffix
         in if isDigit c
              then Text.drop 1 suffix
              else parseVersion . Text.drop 1 $ suffix

parseName :: Text -> Text
parseName s = Text.dropEnd ((Text.length . parseVersion $ s) + 1) s

data PackageMeta = PackageMeta
  { packageMetaPlatforms :: Maybe [Text]
  , packageMetaMaintainers :: Maybe [Text]
  , packageMetaDescription :: Maybe Text
  , packageMetaLicense :: Maybe [PackageLicense]
  , packageMetaPosition :: Maybe Text
  , packageMetaHomepage :: Maybe [Text]
  , packageMetaLongDescription :: Maybe Text
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
  parseJSON x = panic . show $ x

data LicenseDetails = LicenseDetails
  { detailedLicenseShortName :: Maybe Text
  , detailedLicenseFullName :: Maybe Text
  , detailedLicenseUrl :: Maybe Text
  , detailedLicenseSpdxId :: Maybe Text
  } deriving (Eq, Generic, Show)

instance FromJSON LicenseDetails where
  parseJSON (Object o) =
    LicenseDetails <$> o .:? "shortName" <*> o .:? "fullName" <*> o .:? "url" <*>
    o .:? "spdxId"
  parseJSON x = panic . show $ x

instance ToJSON LicenseDetails where
  toJSON =
    genericToJSON $ aesonDrop (length ("LicenseDetails" :: String)) camelCase

parsePackages :: (MonadIO m) => FilePath -> m (HashMap Text Package)
parsePackages path = do
  s <- liftIO . Bytes.readFile $ path
  let parser =
        withObject "Packages" $ \o ->
          sequenceA (parseJSON <$> (Vec.fromList . HashMap.elems $ o))
  let mRet = join (parseEither parser <$> eitherDecodeStrict s)
  case mRet of
    Left e -> panic . toS $ e
    Right ret -> return . Vec.foldl' go HashMap.empty $ ret
  where
    go :: HashMap Text Package -> Package -> HashMap Text Package
    go acc x = HashMap.insert (packageName x) x acc

packageUrl :: Package -> Maybe Text
packageUrl pkg =
  let position = packageMetaPosition . packageMeta $ pkg
      path = fst . Text.breakOn ":" . snd . Text.breakOn "/pkgs" <$> position
  in ("https://github.com/NixOS/nixpkgs/blob/release-17.03/" <>) <$> path
