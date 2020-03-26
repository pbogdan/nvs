{-# LANGUAGE DeriveGeneric #-}

module Nvd.Cpe
  ( Cpe(..)
  , cpeMatch
  )
where

import           Protolude               hiding ( (<&&>) )

import           Data.Aeson
import           Data.Aeson.Types               ( typeMismatch )
import           Nixpkgs.Packages.Types
import           Nvd.Cpe.Uri

data Cpe = Cpe
  { cpeVulnerable :: Bool
  , cpePreviousVersions :: Maybe Bool
  , cpeCpeUri :: CpeUri
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON Cpe where
  parseJSON (Object o) =
    Cpe <$> o .: "vulnerable" <*> o .:? "previousVersions" <*> o .: "cpe23Uri"
  parseJSON x = typeMismatch "Cpe" x

instance ToJSON Cpe where

(<&&>) :: Maybe Bool -> Maybe Bool -> Maybe Bool
(<&&>) = liftA2 (&&)

cpeMatchExact :: (PackageName, PackageVersion) -> Cpe -> Maybe Bool
cpeMatchExact x cpe =
  cpeUriMatch x (cpeCpeUri cpe) <&&> Just (cpeVulnerable cpe)

cpeMatch :: (PackageName, PackageVersion) -> Cpe -> Maybe Bool
cpeMatch x@(pName, pVersion) cpe = case cpePreviousVersions cpe of
  Just False -> cpeMatchExact x cpe
  Nothing    -> cpeMatchExact x cpe
  Just True  -> do
    pVersionParsed   <- mkParsedPackageVersion pVersion
    uriVersion       <- cpeUriPackageVersion . cpeCpeUri $ cpe
    uriVersionParsed <- mkParsedPackageVersion uriVersion
    uriName          <- cpeUriPackageName . cpeCpeUri $ cpe
    return
      (  pName
      == uriName
      && pVersionParsed
      <= uriVersionParsed
      && cpeVulnerable cpe
      )
