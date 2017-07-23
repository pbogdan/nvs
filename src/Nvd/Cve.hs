{-# LANGUAGE DeriveGeneric #-}

module Nvd.Cve
  ( Cve(..)
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
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String (String)
import           Data.Vector (Vector)
import qualified Data.Vector as Vec

data Cve = Cve
  { cveId :: Text
  , cveAffects :: [VendorData]
  , cveDescription :: Text
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

data VendorData = VendorData
  { vendorName :: Text
  , vendorProduct :: [VendorProduct]
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON VendorData where
  parseJSON (Object o) =
    VendorData <$> o .: "vendor_name" <*>
    (o .: "product" >>= (.: "product_data"))
  parseJSON x = panic . show $ x

instance ToJSON VendorData where
  toJSON = genericToJSON $ aesonDrop (length ("VendorData" :: String)) camelCase

data VendorProduct = VendorProduct
  { vendorProductName :: Text
  , vendorProductVersion :: [Text]
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

parseCves :: MonadIO m => FilePath -> m (Vector Cve)
parseCves path = do
  s <- liftIO . Bytes.readFile $ path
  let parser = withObject "cves" $ \o -> o .: "CVE_Items" >>= parseJSON
  let mRet = join (parseEither parser <$> eitherDecodeStrict s)
  return $ either (panic . toS) identity mRet

cveProducts :: Cve -> [(Text, Text)]
cveProducts cve =
  let affected = cveAffects cve
      products = concatMap vendorProduct affected
  in concatMap
       (\p -> zip (repeat . vendorProductName $ p) (vendorProductVersion p))
       products

cvesByProduct :: Vector Cve -> HashMap (Text, Text) (Set Cve)
cvesByProduct = Vec.foldl' go HashMap.empty
  where
    go ::
         HashMap (Text, Text) (Set Cve) -> Cve -> HashMap (Text, Text) (Set Cve)
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
