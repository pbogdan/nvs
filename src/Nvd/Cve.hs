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
  , Cve(..)
  , VendorData(..)
  , VendorProduct(..)
  , cveProducts
  , parseCves
  , cvesByPackage
  , cvesForPackage
  , vulnsFor
  , vulnsFor'
  ) where

import           Protolude

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Types
import qualified Data.ByteString as Bytes
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String (String, IsString(..))
import qualified Data.Text as Text
import           Data.Vector (Vector)
import qualified Data.Vector as Vec
import           Nixpkgs.Packages
import           Nixpkgs.Packages.Aliases
import           Nixpkgs.Packages.Types
import           Nvd.Cpe.Configuration
import           Nvd.Cve.Types
import           Nvs.Types
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

-- | Representation of a CVE parsed out of NVD feed. Currently only fields
-- directly needed by this project are extracted, with the rest of them
-- discarded.
data Cve a = Cve
  { cveId :: CveId -- ^ The ID of the CVE such as CVE-2016-5253.
  , cveDescription :: Text -- ^ Description of the CVE.
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
  Cve <$> (cve >>= (.: "CVE_data_meta") >>= (.: "ID")) <*> pure desc
parseCveCommon x = typeMismatch "parseCveCommon" x

instance FromJSON (Cve VendorData) where
  parseJSON js@(Object o) = do
    let cve = o .: "cve"
    parseCveCommon js <*>
      (cve >>= (.: "affects") >>= (.: "vendor") >>= (.: "vendor_data"))
  parseJSON _ = mzero

instance ToJSON a => ToJSON (Cve a) where
  toJSON = genericToJSON $ aesonDrop (length ("Cve" :: String)) camelCase

instance FromJSON (Cve CpeConfiguration) where
  parseJSON js@(Object o) =
    parseCveCommon js <*> ((o .: "configurations") >>= (.: "nodes"))
  parseJSON _ = mzero

-- | Name and list of vendor's products.
data VendorData = VendorData
  { vendorName :: Text -- ^ The name of the vendor.
  , vendorProduct :: [VendorProduct] -- ^ List of vendor's products.
  } deriving (Eq, Generic, Ord, Show)

instance Affects VendorData where
  packages = map vendorProductName . vendorProduct
  isAffected x p =
    let products = vendorProduct x
        items =
          concatMap
            (\y -> zip (repeat . vendorProductName $ y) (vendorProductVersion y))
            products
    in p `elem` items

cveProducts :: Cve VendorData -> [(PackageName, PackageVersion)]
cveProducts cve =
  let affected = cveAffects cve
      products = concatMap vendorProduct affected
  in concatMap
       (\p -> zip (repeat . vendorProductName $ p) (vendorProductVersion p))
       products

instance FromJSON VendorData where
  parseJSON (Object o) =
    VendorData <$> o .: "vendor_name" <*>
    (o .: "product" >>= (.: "product_data"))
  parseJSON _ = mzero

instance ToJSON VendorData where
  toJSON = genericToJSON $ aesonDrop (length ("VendorData" :: String)) camelCase

-- | Description of a vendor's product, including its name and a list of
-- versions.
data VendorProduct = VendorProduct
  { vendorProductName :: PackageName -- ^ Name of the product.
  , vendorProductVersion :: [PackageVersion] -- ^ List of product versions.
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON VendorProduct where
  parseJSON (Object o) = do
    meh <- o .: "version" >>= (.: "version_data")
    vers <-
      Vec.forM meh $ \x ->
        case x of
          Object o' -> o' .: "version_value"
          _ -> fail "version_data must be a list of objects"
    VendorProduct <$> o .: "product_name" <*> (pure . Vec.toList $ vers)
  parseJSON _ = mzero

instance ToJSON VendorProduct where
  toJSON =
    genericToJSON $ aesonDrop (length ("VendorProduct" :: String)) camelCase

preParse :: (MonadIO m, MonadError NvsError m, FromJSON a) => FilePath -> m a
preParse path = do
  s <- liftIO . Bytes.readFile $ path
  let parser = withObject "cves" $ \o -> o .: "CVE_Items" >>= parseJSON
  let mRet = join (parseEither parser <$> eitherDecodeStrict s)
  either (throwError . FileParseError path . toS) return mRet

-- | Utility function for parsing CVE information out of NVD JSON feed.
parseCves ::
     (Affects a, FromJSON (Cve a), Ord a, MonadError NvsError m, MonadIO m)
  => FilePath -- ^ Path to the NVD JSON feed file
  -> m (HashMap PackageName (Set (Cve a)))
parseCves path = do
  pkgs <- preParse path
  return . cvesByPackage $ pkgs

cvesByPackage ::
     (Ord a, Affects a) => Vector (Cve a) -> HashMap PackageName (Set (Cve a))
cvesByPackage = foldl' go HashMap.empty
  where
    go ::
         (Ord a, Affects a)
      => HashMap PackageName (Set (Cve a))
      -> Cve a
      -> HashMap PackageName (Set (Cve a))
    go acc cve =
      let pkgs = concatMap packages cve
          go' acc' x =
            let current = HashMap.lookup x acc'
                updated =
                  case current of
                    Nothing -> Set.singleton cve
                    Just cves -> Set.insert cve cves
            in HashMap.insert x updated acc'
      in HashMap.unionWith Set.union (foldl' go' HashMap.empty pkgs) acc

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

-- @TODO: it doesn't handle aliases correctly when producing the reduced package
-- set
vulnsFor' ::
     (Affects (Cve a), Affects a, Ord a, Show a)
  => Cve a
  -> PackageSet
  -> HashMap PackageName PackageAlias
  -> [(Package, Set (Cve a))]
vulnsFor' cve (KeyedSet pkgs) aliases =
  let candidates =
        foldl' (\m n -> HashMap.insert n (Set.singleton cve) m) HashMap.empty .
        packages $
        cve
      pkgs' =
        foldl'
          (\m (name, set) -> HashMap.insertWith Set.union name set m)
          HashMap.empty .
        mapMaybe (`lookupWithKey` pkgs) . packages $
        cve
  in vulnsFor (KeyedSet pkgs') aliases candidates
  where
    lookupWithKey k m = (,) <$> pure k <*> HashMap.lookup k m
