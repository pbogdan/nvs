{-# LANGUAGE ScopedTypeVariables #-}

module Nvd.Cve where

import           Protolude

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString as Bytes
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Vector (Vector)
import qualified Data.Vector as Vec

data Cve = Cve
  { cveId :: Text
  , cveAffects :: [VendorData]
  , cveDescription :: Text
  } deriving (Eq, Show)

instance FromJSON Cve where
  parseJSON (Object o) = do
    dd <- o .: "cve" >>= (.: "description") >>= (.: "description_data")
    desc <-
      case Vec.head dd of
        Object o' -> o' .: "value"
        _ -> fail "lolz"
    Cve <$> (o .: "cve" >>= (.: "CVE_data_meta") >>= (.: "ID")) <*>
      (o .: "cve" >>= (.: "affects") >>= (.: "vendor") >>= (.: "vendor_data")) <*>
      pure desc
  parseJSON x = panic . show $ x

data VendorData = VendorData
  { vendorName :: Text
  , vendorProduct :: [VendorProduct]
  } deriving (Eq, Show)

instance FromJSON VendorData where
  parseJSON (Object o) =
    VendorData <$> o .: "vendor_name" <*>
    (o .: "product" >>= (.: "product_data"))
  parseJSON x = panic . show $ x

data VendorProduct = VendorProduct
  { vendorProductName :: Text
  , vendorProductVersion :: [Text]
  } deriving (Eq, Show)

instance FromJSON VendorProduct where
  parseJSON (Object o) = do
    meh <- o .: "version" >>= (.: "version_data")
    vers <-
      Vec.forM meh $ \x ->
        case x of
          Object o' -> o' .: "version_value"
          _ -> fail "lolz"
    VendorProduct <$> o .: "product_name" <*> (pure . Vec.toList $ vers)
  parseJSON x = panic . show $ x

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

cvesByProduct :: Vector Cve -> HashMap (Text, Text) Cve
cvesByProduct = Vec.foldl' go HashMap.empty
  where
    go :: HashMap (Text, Text) Cve -> Cve -> HashMap (Text, Text) Cve
    go acc cve =
      let products = cveProducts cve
          go' acc' x = HashMap.insert x cve acc'
      in HashMap.union (foldl' go' HashMap.empty products) acc
