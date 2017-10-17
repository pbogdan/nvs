{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Nvd.Cpe.Uri
  ( CpePart(..)
  , CpeValue(..)
  , CpeUri(..)
  , cpeUriMatch
  , cpeUriPackageName
  , cpeUriPackageVersion
  ) where

import           Protolude hiding (Any, Product)

import           Control.Monad (fail)
import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import qualified Data.Text as Text
import           GHC.TypeLits
import           Nixpkgs.Packages.Types

data CpePart
  = Application
  | OS
  | Hardware
  | Unknown
  deriving (Eq, Ord, Show)

data CpeValue a
  = Any
  | NA
  | CpeValue Text
  deriving (Eq, Generic, Ord, Show)

instance (ToJSON a) => ToJSON (CpeValue a)

data CpeUri = CpeUri
  { cpePart :: CpePart
  , cpeVendor :: CpeValue Vendor
  , cpeProduct :: CpeValue Product
  , cpeVersion :: CpeValue Version
  } deriving (Eq, Generic, Show)

instance Ord CpeUri where
  x `compare` y = cpeVersion x `compare` cpeVersion y

instance FromJSON CpeUri where
  parseJSON (String s) = either (fail . toS) pure (parseCpeUri s)
  parseJSON x = typeMismatch "CpeUri" x

instance ToJSON CpeUri where
  toJSON _ = object []

data Vendor
data Product
data Version
data Update
data Edition
data Language
data SwEdition
data TargetSw
data TargetHw
data Other

type family SegmentIndex (a :: *) :: Nat where
  SegmentIndex Vendor = 3
  SegmentIndex Product = 4
  SegmentIndex Version = 5
  SegmentIndex Update = 6
  SegmentIndex Edition = 7
  SegmentIndex Language = 8
  SegmentIndex SwEdition = 9
  SegmentIndex TargetSw = 10
  SegmentIndex TargetHw = 11
  SegmentIndex Other = 12

parseCpeUri :: Text -> Either Text CpeUri
parseCpeUri s =
  let parts = Text.splitOn ":" s
  in CpeUri <$> parseCpePart parts <*> parseCpeValue (Proxy :: Proxy 3) parts <*>
     parseCpeValue (Proxy :: Proxy 4) parts <*>
     parseCpeValue (Proxy :: Proxy 5) parts

parseCpeValue ::
     (KnownNat b, b ~ SegmentIndex a)
  => proxy b
  -> [Text]
  -> Either Text (CpeValue a)
parseCpeValue p parts =
  let index = natVal p
  in cpeUriSegment (fromIntegral index) parts >>= parse
  where
    parse s' =
      case s' of
        "" -> Right Any
        "-" -> Right NA
        _ -> Right . CpeValue $ s'

cpeUriSegment :: Int -> [Text] -> Either Text Text
cpeUriSegment i parts =
  let item = head . drop i $ parts
  in case item of
       Nothing -> Left $ "Segment " <> show i <> " doesn't exist"
       Just x -> Right x

parseCpePart :: [Text] -> Either Text CpePart
parseCpePart parts =
  case cpeUriSegment 2 parts of
    Right "a" -> Right Application
    Right "o" -> Right OS
    Right "h" -> Right Hardware
    Right _ -> Right Unknown
    Left e -> Left e

cpeUriPackageName :: CpeUri -> Maybe PackageName
cpeUriPackageName uri =
  case cpeProduct uri of
    NA -> Nothing
    Any -> Nothing
    CpeValue v -> Just . PackageName . toS $ v

cpeUriPackageVersion :: CpeUri -> Maybe PackageVersion
cpeUriPackageVersion uri =
  case cpeVersion uri of
    NA -> Nothing
    Any -> Nothing
    CpeValue v -> Just . PackageVersion . toS $ v

cpeUriMatch :: (PackageName, PackageVersion) -> CpeUri -> Maybe Bool
cpeUriMatch (name, version) uri = do
  cName <- cpeUriPackageName uri
  cVersion <- cpeUriPackageVersion uri
  return (name == cName && version == cVersion)
