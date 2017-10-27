{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Nvd.Cve
Description : Utilities to interface with JSON feeds provided by NVD.
Copyright   : (c) Piotr Bogdan, 2017
License     : BSD3
Maintainer  : ppbogdan@gmail.com
Stability   : experimental
Portability : Unknown

Utilities to interface with JSON feeds provided by NVD.

-}

module Nvd.Cve
  ( CveId(..)
  , displayCveId
  , Severity(..)
  , Cve(..)
  , vulnsFor'
  ) where

import           Protolude hiding (transpose)

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Types
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String (String, IsString(..))
import qualified Data.Text as Text
import           Data.Time
import qualified Data.Vector as Vec
import           Nixpkgs.Packages
import           Nixpkgs.Packages.Aliases
import           Nixpkgs.Packages.Types
import           Nvd.Cpe.Configuration
import           Nvd.Cve.Types
import           Text.Read (read)

-- @TODO: there should be validation of the CVE ID format

-- | CVE ID.
newtype CveId =
  CveId Text
  deriving (Eq, Generic, Show)

instance FromJSON CveId where
  parseJSON (String s) = pure . CveId $ s
  parseJSON _ = mzero

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
  (<=) x y =
    if cveIdYear x == cveIdYear y
      then cveIdId x <= cveIdId y
      else cveIdYear x <= cveIdYear y

data Severity
  = Low
  | Medium
  | High
  | Critical
  deriving (Eq, Generic, Ord, Show)

instance FromJSON Severity where
  parseJSON (String "LOW") = pure Low
  parseJSON (String "MEDIUM") = pure Medium
  parseJSON (String "HIGH") = pure High
  parseJSON (String "CRITICAL") = pure Critical
  parseJSON x = typeMismatch "Severity" x

instance ToJSON Severity

-- | Representation of a CVE parsed out of NVD feed. Currently only fields
-- directly needed by this project are extracted, with the rest of them
-- discarded.
data Cve a = Cve
  { cveId :: CveId -- ^ The ID of the CVE such as CVE-2016-5253.
  , cveDescription :: Text -- ^ Description of the CVE.
  , cveSeverity :: Maybe Severity
  , cvePublished :: UTCTime
  , cveAffects :: [a] -- ^ List of affected vendors and products.
  } deriving (Eq, Foldable, Functor, Generic, Ord, Show)

instance Affects a => Affects (Cve a) where
  packages = concatMap packages
  isAffected x p =
    let as = cveAffects x
    in any (`isAffected` p) as

parseCveCommon :: Value -> Parser ([a] -> Cve a)
parseCveCommon (Object o) = do
  let cve = o .: "cve"
  dd <- cve >>= (.: "description") >>= (.: "description_data")
  desc <-
    case Vec.head dd of
      Object o' -> o' .: "value"
      _ -> fail "description_data must be an object"
  impact <- o .: "impact"
  mmetrics <-
    impact .: "baseMetricV3" <|> impact .: "baseMetricV2" <|> pure Nothing
  severity <-
    case mmetrics of
      Nothing -> pure Nothing
      Just metrics ->
        (metrics .: "cvssV3" >>= (.: "baseSeverity")) <|>
        (metrics .: "severity")
  published <-
    parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%MZ" =<< o .: "publishedDate"
  Cve <$> (cve >>= (.: "CVE_data_meta") >>= (.: "ID")) <*> pure desc <*>
    pure severity <*>
    pure published
parseCveCommon x = typeMismatch "parseCveCommon" x

instance ToJSON a => ToJSON (Cve a) where
  toJSON = genericToJSON $ aesonDrop (length ("Cve" :: String)) camelCase

instance FromJSON (Cve CpeConfiguration) where
  parseJSON js@(Object o) =
    parseCveCommon js <*> ((o .: "configurations") >>= (.: "nodes"))
  parseJSON _ = mzero

cvesForPackage ::
     (Affects (Cve a), Ord a)
  => Package
  -> HashMap PackageName PackageAlias
  -> HashMap PackageName (Set (Cve a))
  -> (Package, Set (Cve a))
cvesForPackage pkg aliases cves =
  let pVersion = packageVersion pkg
      pName = packageName pkg
      pAliases =
        fromMaybe [] (packageAliasAliases <$> HashMap.lookup pName aliases)
      terms = pName : pAliases
      queries =
        foldr Set.union Set.empty . catMaybes $
        [HashMap.lookup term cves | term <- terms]
      candidates = zip terms (repeat pVersion)
      fns = map (flip isAffected) candidates
      matches =
        Set.filter
          (\cve ->
             let ret = map (\fn -> fn cve) fns
             in getAny . foldMap Any $ ret)
          queries
  in (pkg, matches)

vulnsFor ::
     (Affects (Cve a), Affects a, Ord a)
  => PackageSet
  -> HashMap PackageName PackageAlias
  -> HashMap PackageName (Set (Cve a))
  -> [(Package, Set (Cve a))]
vulnsFor pkgs aliases cves =
  foldl' (\acc pkg -> cvesForPackage pkg aliases cves : acc) [] pkgs

vulnsFor' ::
     (Affects (Cve a), Affects a, Ord a, Show a)
  => Cve a
  -> PackageSet
  -> HashMap PackageName PackageAlias
  -> [(Package, Set (Cve a))]
vulnsFor' cve (KeyedSet pkgs) aliases =
  let aliases' = mapMaybe (`HashMap.lookup` transpose aliases) . packages $ cve
      candidates =
        foldl' (\m n -> HashMap.insert n (Set.singleton cve) m) HashMap.empty .
        packages $
        cve
      pkgs' =
        foldl'
          (\m (name, set) -> HashMap.insertWith Set.union name set m)
          HashMap.empty .
        mapMaybe (`lookupWithKey` pkgs) $
        (aliases' <> packages cve)
  in vulnsFor (KeyedSet pkgs') aliases candidates
  where
    lookupWithKey k m = (,) <$> pure k <*> HashMap.lookup k m
    transpose ::
         HashMap PackageName PackageAlias -> HashMap PackageName PackageName
    transpose =
      foldl'
        (\acc x ->
           foldr (`HashMap.insert` packageAliasPackage x) acc $
           packageAliasAliases x)
        HashMap.empty
