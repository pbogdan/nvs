{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nvd.Cve
  ( CveId(..)
  , displayCveId
  , Severity(..)
  , Cve(..)
  , matchMany
  )
where

import           Protolude               hiding ( packageName
                                                , transpose
                                                )

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Types
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import qualified Data.Set                      as Set
import           Data.String                    ( String
                                                , IsString(..)
                                                )
import qualified Data.Text                     as Text
import           Data.Time
import qualified Data.Vector                   as Vec
import           Nixpkgs.Packages
import           Nixpkgs.Packages.Types
import           Nvd.Cpe
import           Nvd.Cpe.Configuration          ( Configuration )
import qualified Nvd.Cpe.Configuration         as Configuration
                                                ( collectNames
                                                , match
                                                )
import           Text.Read                      ( read )

newtype CveId =
  CveId Text
  deriving (Eq, Generic, Show)

instance FromJSON CveId where
  parseJSON (String s) = pure . CveId $ s
  parseJSON _          = mzero

instance ToJSON CveId where
  toJSON (CveId id) = String id

instance IsString CveId where
  fromString = CveId . toS

cveIdYear :: CveId -> Int
cveIdYear (CveId id) = read . toS . Text.take 4 . Text.drop 4 $ id

cveIdId :: CveId -> Int
cveIdId (CveId id) = read . toS . Text.drop 9 $ id

-- | Helper to convert CveId into textual representation.
displayCveId :: CveId -> Text
displayCveId (CveId id) = id

instance Ord CveId where
  (<=) x y = if cveIdYear x == cveIdYear y
    then cveIdId x <= cveIdId y
    else cveIdYear x <= cveIdYear y

data Severity
  = Low
  | Medium
  | High
  | Critical
  deriving (Eq, Generic, Ord, Show)

instance FromJSON Severity where
  parseJSON (String "LOW"     ) = pure Low
  parseJSON (String "MEDIUM"  ) = pure Medium
  parseJSON (String "HIGH"    ) = pure High
  parseJSON (String "CRITICAL") = pure Critical
  parseJSON x                   = typeMismatch "Severity" x

instance ToJSON Severity where
  toJSON Low      = String "LOW"
  toJSON Medium   = String "MEDIUM"
  toJSON High     = String "HIGH"
  toJSON Critical = String "CRITICAL"

data Cve a = Cve
  { cveId :: CveId
  , cveDescription :: Text
  , cveSeverity :: Maybe Severity
  , cvePublished :: UTCTime
  , cveAffects :: [a]
  } deriving (Eq, Foldable, Functor, Generic, Ord, Show)

parseCveCommon :: Value -> Parser ([a] -> Cve a)
parseCveCommon (Object o) = do
  let cve = o .: "cve"
  dd   <- cve >>= (.: "description") >>= (.: "description_data")
  desc <- case Vec.head dd of
    Object o' -> o' .: "value"
    _         -> fail "description_data must be an object"
  impact   <- o .: "impact"
  mmetrics <-
    impact .: "baseMetricV3" <|> impact .: "baseMetricV2" <|> pure Nothing
  severity <- case mmetrics of
    Nothing -> pure Nothing
    Just metrics ->
      (metrics .: "cvssV3" >>= (.: "baseSeverity")) <|> (metrics .: "severity")
  published <-
    parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%MZ" =<< o .: "publishedDate"
  Cve
    <$> (cve >>= (.: "CVE_data_meta") >>= (.: "ID"))
    <*> pure desc
    <*> pure severity
    <*> pure published
parseCveCommon x = typeMismatch "parseCveCommon" x

instance ToJSON a => ToJSON (Cve a) where
  toJSON = genericToJSON $ aesonDrop (length ("Cve" :: String)) camelCase

instance FromJSON (Cve (Configuration Cpe)) where
  parseJSON js@(Object o) =
    parseCveCommon js <*> ((o .: "configurations") >>= (.: "nodes"))
  parseJSON _ = mzero

match :: (PackageName, PackageVersion) -> Cve (Configuration Cpe) -> Bool
match pkg@(_, _) = or . cveAffects . fmap (Configuration.match pkg)

{-
@TODO: see if I can incorporate adding extra package names that map some known naming convention
into "proper" package names, for example `firefox-bin` -> `firefox`, `foo-unstable` -> `foo`
-}
matchMany
  :: Ord a
  => HashMap PackageName (Set (Package a))
  -> Cve (Configuration Cpe)
  -> Set (Package a)
matchMany pkgs cve =
  let configurations    = cveAffects cve
      affectedNames     = concatMap Configuration.collectNames configurations
      candidatePackages = foldr
        (\n xs -> Set.union xs (HashMap.lookupDefault Set.empty n pkgs))
        mempty
        affectedNames
      affectedPackages = Set.filter
        (\pkg -> match (packageName pkg, packageVersion pkg) cve)
        candidatePackages
  in  affectedPackages
