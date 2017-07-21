module Distribution.Package where

import           Protolude

import           Data.Aeson
import Data.Char
import qualified Data.Text as Text

{-
"nixos.paperkey" : {
      "system" : "x86_64-linux",
      "name" : "paperkey-1.3",
      "meta" : {
         "platforms" : [
            "i686-linux",
            "x86_64-linux",
            "armv5tel-linux",
            "armv6l-linux",
            "armv7l-linux",
            "aarch64-linux",
            "mips64el-linux"
         ],
         "maintainers" : [
            "Sven Keidel <svenkeidel@gmail.com>"
         ],
         "description" : "Store OpenPGP or GnuPG on paper",
         "license" : {
            "shortName" : "gpl2",
            "fullName" : "GNU General Public License v2.0 only",
            "url" : "http://spdx.org/licenses/GPL-2.0",
            "spdxId" : "GPL-2.0"
         },
         "position" : "/home/pbogdan/nixpkgs/pkgs/tools/security/paperkey/default.nix:16",
         "outputsToInstall" : [
            "out"
         ],
         "homepage" : "http://www.jabberwocky.com/software/paperkey/",
         "longDescription" : "A reasonable way to achieve a long term backup of OpenPGP (GnuPG, PGP, etc)\nkeys is to print them out on paper. Paper and ink have amazingly long\nretention qualities - far longer than the magnetic or optical means that\nare generally used to back up computer data.\n"
      }
   },

-}

data Package = Package
  { packageSystem :: Text
  , packageName :: Text
  , packageVersion :: Text
  , packageMeta :: PackageMeta
  } deriving (Eq, Show)

instance FromJSON Package where
  parseJSON (Object o) =
    Package <$> o .: "system" <*> (parseName <$> o .: "name") <*>
    (parseVersion <$> o .: "name") <*>
    o .: "meta"
  parseJSON _ = mzero

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

-- @TODO: license can be a single license or an array of licenses
data PackageMeta = PackageMeta
  { packageMetaPlatforms :: Maybe [Text]
  , packageMetaMaintainers :: Maybe [Text]
  , packageMetaDescription :: Maybe Text
  , packageMetaLicense :: Maybe [PackageLicense]
  , packageMetaPosition :: Maybe Text
  , packageMetaHomepage :: Maybe [Text]
  , packageMetaLongDescription :: Maybe Text
  } deriving (Eq, Show)

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

data PackageLicense
  = DetailedLicense LicenseDetails
  | BasicLicense Text
  deriving (Eq, Show)

instance FromJSON PackageLicense where
  parseJSON js@(Object _) = DetailedLicense <$> parseJSON js
  parseJSON (String s) = pure . BasicLicense $ s
  parseJSON x = panic . show $ x

data LicenseDetails = LicenseDetails
  { detailedLicenseShortName :: Maybe Text
  , detailedLicenseFullName :: Maybe Text
  , detailedLicenseUrl :: Maybe Text
  , detailedLicenseSpdxId :: Maybe Text
  } deriving (Eq, Show)

instance FromJSON LicenseDetails where
  parseJSON (Object o) =
    LicenseDetails <$> o .:? "shortName" <*> o .:? "fullName" <*> o .:? "url" <*>
    o .:? "spdxId"
  parseJSON x = panic . show $ x
