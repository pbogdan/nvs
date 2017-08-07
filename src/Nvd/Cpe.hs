module Nvd.Cpe where

import Protolude

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Nixpkgs.Packages.Types
import Nvd.Cpe.Uri

data Cpe = Cpe
  { cpeVulnerable :: Bool
  , cpePreviousVersions :: Maybe Bool
  , cpeCpeUri :: CpeUri
  } deriving (Eq, Ord, Show)

instance FromJSON Cpe where
  parseJSON (Object o) =
    Cpe <$> o .: "vulnerable" <*> o .:? "previousVersions" <*> o .: "cpe23Uri"
  parseJSON x = typeMismatch "Cpe" x

instance ToJSON Cpe where
  toJSON _ = undefined

(<&&>) :: Maybe Bool -> Maybe Bool -> Maybe Bool
(<&&>) = liftA2 (&&)

cpeMatch :: (PackageName, PackageVersion) -> Cpe -> Maybe Bool
cpeMatch x@(pName, pVersion) cpe =
  case cpePreviousVersions cpe of
    Just False -> cpeUriMatch x (cpeCpeUri cpe) <&&> Just (cpeVulnerable cpe)
    Nothing -> cpeUriMatch x (cpeCpeUri cpe) <&&> Just (cpeVulnerable cpe)
    Just True -> do
      pVersionParsed <- mkParsedPackageVersion pVersion
      uriVersion <- cpeUriPackageVersion . cpeCpeUri $ cpe
      uriVersionParsed <- mkParsedPackageVersion uriVersion
      uriName <- cpeUriPackageName . cpeCpeUri $ cpe
      return
        (pName == uriName &&
         pVersionParsed <= uriVersionParsed && cpeVulnerable cpe)
