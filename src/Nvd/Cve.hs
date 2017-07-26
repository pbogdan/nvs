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

module Nvd.Cve
  ( CveId
  , displayCveId
  , Cve(..)
  , VendorData(..)
  , parseCves
  , cvesByProduct
  ) where

import           Protolude

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Types
import qualified Data.ByteString as Bytes
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.List ((!!))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String (String)
import qualified Data.Text as Text
import           Data.Vector (Vector)
import qualified Data.Vector as Vec
import           Nixpkgs.Packages.Types
import           Text.Read (read)

-- | CVE ID.
newtype CveId =
  CveId Text
  deriving (Eq, Generic, Show)

instance FromJSON CveId where
  parseJSON (String s) = pure . CveId $ s
  parseJSON _ = mzero

instance ToJSON CveId where
  toJSON (CveId id) = String id

cveIdYear :: CveId -> Int
cveIdYear (CveId id) =
  let parts = Text.splitOn "-" id
  in read . toS $ (parts !! 1)

cveIdId :: CveId -> Int
cveIdId (CveId id) =
  let parts = Text.splitOn "-" id
  in read . toS $ (parts !! 2)

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
data Cve = Cve
  { cveId :: CveId -- ^ The ID of the CVE such as CVE-2016-5253.
  , cveAffects :: [VendorData] -- ^ List of affected vendors and products.
  , cveDescription :: Text -- ^ Description of the CVE.
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON Cve where
  parseJSON (Object o) = do
    let cve = o .: "cve"
    dd <- cve >>= (.: "description") >>= (.: "description_data")
    desc <-
      case Vec.head dd of
        Object o' -> o' .: "value"
        _ -> fail "description_data must be an object"
    Cve <$> (cve >>= (.: "CVE_data_meta") >>= (.: "ID")) <*>
      (cve >>= (.: "affects") >>= (.: "vendor") >>= (.: "vendor_data")) <*>
      pure desc
  parseJSON x = panic . show $ x

instance ToJSON Cve where
  toJSON = genericToJSON $ aesonDrop (length ("Cve" :: String)) camelCase

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

-- | Utility function for parsing CVE information out of NVD JSON feed.
parseCves ::
     MonadIO m
  => FilePath -- ^ Path to the NVD JSON feed file
  -> m (Vector Cve)
parseCves path = do
  s <- liftIO . Bytes.readFile $ path
  let parser = withObject "cves" $ \o -> o .: "CVE_Items" >>= parseJSON
  let mRet = join (parseEither parser <$> eitherDecodeStrict s)
  return $ either (panic . toS) identity mRet

-- | Given a Cve return a list of tuples representing products' names and
-- versions. For example given a Cve an entry in the list may look as follows:
--
-- > ("ffmpeg", "2.8.11")
cveProducts :: Cve -> [(PackageName, PackageVersion)]
cveProducts cve =
  let affected = cveAffects cve
      products = concatMap vendorProduct affected
  in concatMap
       (\p -> zip (repeat . vendorProductName $ p) (vendorProductVersion p))
       products

-- | Given a vector of CVEs produced by 'parseCves' return a hash map keyed off
-- (name, version) tuple where the values are sets of CVEs that affect that
-- particular name / version variant. An example entry may contain
--
-- > ("ffmpeg", "2.8.11")
--
-- key, with the corresponding value being a set of CVEs that affect
-- ffmpeg-2.8.11.
cvesByProduct :: Vector Cve -> HashMap (PackageName, PackageVersion) (Set Cve)
cvesByProduct = Vec.foldl' go HashMap.empty
  where
    go ::
         HashMap (PackageName, PackageVersion) (Set Cve)
      -> Cve
      -> HashMap (PackageName, PackageVersion) (Set Cve)
    go acc cve =
      let products = cveProducts cve
          go' acc' x =
            let current = HashMap.lookup x acc'
                updated =
                  case current of
                    Nothing -> Set.singleton cve
                    Just cves -> Set.insert cve cves
            in HashMap.insert x updated acc'
      in HashMap.union (foldl' go' HashMap.empty products) acc
