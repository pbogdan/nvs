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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Nvd.Cve
  ( CveId
  , displayCveId
  , Cve(..)
  , VendorData(..)
  , parseCves
  , cvesByPackage
  , cvesForPackage
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
import           Text.Read (read)

-- @TODO: get rid of unsafe list lookups with !! and there should be validation
-- of the CVE ID format as well

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
data NvdCve = NvdCve
  { nvdCveId :: CveId -- ^ The ID of the CVE such as CVE-2016-5253.
  , nvdCveAffects :: [VendorData] -- ^ List of affected vendors and products.
  , nvdCveDescription :: Text -- ^ Description of the CVE.
  } deriving (Eq, Generic, Show)

instance FromJSON NvdCve where
  parseJSON (Object o) = do
    let cve = o .: "cve"
    dd <- cve >>= (.: "description") >>= (.: "description_data")
    desc <-
      case Vec.head dd of
        Object o' -> o' .: "value"
        _ -> fail "description_data must be an object"
    NvdCve <$> (cve >>= (.: "CVE_data_meta") >>= (.: "ID")) <*>
      (cve >>= (.: "affects") >>= (.: "vendor") >>= (.: "vendor_data")) <*>
      pure desc
  parseJSON x = panic . show $ x

instance ToJSON NvdCve where
  toJSON = genericToJSON $ aesonDrop (length ("NvdCve" :: String)) camelCase

-- | Name and list of vendor's products.
data VendorData = VendorData
  { vendorName :: Text -- ^ The name of the vendor.
  , vendorProduct :: [VendorProduct] -- ^ List of vendor's products.
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON VendorData where
  parseJSON (Object o) =
    VendorData <$> o .: "vendor_name" <*>
    (o .: "product" >>= (.: "product_data"))
  parseJSON x = panic . show $ x

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
  parseJSON x = panic . show $ x

instance ToJSON VendorProduct where
  toJSON =
    genericToJSON $ aesonDrop (length ("VendorProduct" :: String)) camelCase

-- | Simplified representation of CVE data type retrieved from the JSON feed to
-- be used in the application.
data Cve = Cve
  { cveId :: CveId -- ^ The ID of the CVE such as CVE-2016-5253.
  , cveAffects :: [(PackageName, PackageVersion)] -- ^ List of affected products.
  , cveDescription :: Text -- ^ Description of the CVE.
  } deriving (Eq, Generic, Ord, Show)

nvdCveToCve :: NvdCve -> Cve
nvdCveToCve cve =
  let packages = nvdCveProducts cve
  in Cve (nvdCveId cve) packages (nvdCveDescription cve)

instance ToJSON Cve where
  toJSON = genericToJSON $ aesonDrop (length ("Cve" :: String)) camelCase

-- | Utility function for parsing CVE information out of NVD JSON feed.
parseCves ::
     MonadIO m
  => FilePath -- ^ Path to the NVD JSON feed file
  -> m (HashMap (PackageName, PackageVersion) (Set Cve))
parseCves path = do
  s <- liftIO . Bytes.readFile $ path
  let parser = withObject "cves" $ \o -> o .: "CVE_Items" >>= parseJSON
  let mRet = join (parseEither parser <$> eitherDecodeStrict s)
  return $ either (panic . toS) (cvesByPackage . Vec.map nvdCveToCve) mRet

cvesByPackage :: Vector Cve -> HashMap (PackageName, PackageVersion) (Set Cve)
cvesByPackage = Vec.foldl' go HashMap.empty
  where
    go ::
         HashMap (PackageName, PackageVersion) (Set Cve)
      -> Cve
      -> HashMap (PackageName, PackageVersion) (Set Cve)
    go acc cve =
      let products = cveAffects cve
          go' acc' x =
            let current = HashMap.lookup x acc'
                updated =
                  case current of
                    Nothing -> Set.singleton cve
                    Just cves -> Set.insert cve cves
            in HashMap.insert x updated acc'
      in HashMap.unionWith Set.union (foldl' go' HashMap.empty products) acc

-- | Given a NvdCve return a list of tuples representing products' names and
-- versions. For example given a Cve an entry in the list may look as follows:
--
-- > ("ffmpeg", "2.8.11")
nvdCveProducts :: NvdCve -> [(PackageName, PackageVersion)]
nvdCveProducts cve =
  let affected = nvdCveAffects cve
      products = concatMap vendorProduct affected
  in concatMap
       (\p -> zip (repeat . vendorProductName $ p) (vendorProductVersion p))
       products

cvesForPackage ::
     Package
  -> HashMap PackageName PackageAlias
  -> HashMap (PackageName, PackageVersion) (Set Cve)
  -> (Package, Set Cve)
cvesForPackage pkg aliases cves =
  let pVersion = packageVersion pkg
      pName = packageName pkg
      pAliases =
        fromMaybe [] (packageAliasAliases <$> HashMap.lookup pName aliases)
      terms = [(alias, pVersion) | alias <- pAliases] ++ [(pName, pVersion)]
      queries = [HashMap.lookup term cves | term <- terms]
      matches = foldr Set.union Set.empty $ catMaybes queries
  in (pkg, matches)
