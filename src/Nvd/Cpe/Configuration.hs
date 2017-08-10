{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Nvd.Cpe.Configuration where

import           Protolude

import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           GHC.TypeLits
import           Nixpkgs.Packages.Types
import           Nvd.Cpe
import           Nvd.Cpe.Uri hiding (Any)
import           Nvd.Cve.Types

data Op
  = And
  | Or
  deriving (Eq, Ord, Show)

instance FromJSON Op where
  parseJSON (String "AND") = pure And
  parseJSON (String "OR") = pure Or
  parseJSON x = typeMismatch "Op" x

data Terms a =
  Terms Op
        (NonEmpty a)
  deriving (Eq, Functor, Foldable, Traversable, Ord, Show)

queryTerms :: b -> (b -> a -> Bool) -> Terms a -> Bool
queryTerms y f (Terms op xs) =
  let ys = NE.map (f y) xs
  in case op of
       And -> getAll . foldMap All $ ys
       Or -> getAny . foldMap Any $ ys

type family PayloadKey (a :: *) :: Symbol where
  PayloadKey Cpe = "cpe"

data Payload a (s :: Symbol) where
  Payload :: a -> Payload a (PayloadKey a)

deriving instance Show a => Show (Payload a s)
deriving instance Eq a => Eq (Payload a s)
deriving instance Ord a => Ord (Payload a s)

instance (s ~ PayloadKey a, KnownSymbol s, FromJSON a) =>
         FromJSON (Payload a s) where
  parseJSON js@(Object _) = Payload <$> parseJSON js
  parseJSON x = typeMismatch "Payload" x

instance (s ~ PayloadKey a, KnownSymbol s, FromJSON a) =>
         FromJSON (Terms (Payload a s)) where
  parseJSON (Object o) =
    let prefix = toS (symbolVal (Proxy :: Proxy s))
    in Terms <$> o .: "operator" <*> (parseJSON =<< o .: prefix)
  parseJSON x = typeMismatch "Terms" x

instance ToJSON a => ToJSON (Terms (Payload a s)) where
  toJSON _ = undefined

data Configuration a
  = Leaf a
  | Branch Op
           (NonEmpty (Configuration a))
  deriving (Eq, Foldable, Traversable, Functor, Ord, Show)

instance FromJSON a => FromJSON (Configuration a) where
  parseJSON js@(Object o) = do
    mChildren <- o .:? "children"
    mOp <- o .:? "operator"
    case (mChildren, mOp) of
      (Nothing, Just _) -> Leaf <$> parseJSON js
      (Just cs, Just op) -> Branch <$> pure op <*> parseJSON cs
      (Just _, Nothing) -> typeMismatch "Configration" js
      (Nothing, Nothing) -> typeMismatch "Configration" js
  parseJSON x = typeMismatch "Configuration" x

instance ToJSON a => ToJSON (Configuration a) where
  toJSON _ = undefined

type CpeTerms = Terms (Payload Cpe "cpe")

cpeTermsPackages :: CpeTerms -> [PackageName]
cpeTermsPackages = catMaybes . foldr go []
  where
    go :: Payload Cpe "cpe" -> [Maybe PackageName] -> [Maybe PackageName]
    go (Payload cpe) acc = (cpeUriPackageName . cpeCpeUri $ cpe) : acc

type CpeConfiguration = Configuration (Terms (Payload Cpe "cpe"))

instance Affects CpeConfiguration where
  packages = foldMap cpeTermsPackages
  isAffected = flip cpeConfigurationMatch

cpeConfigurationPackages :: CpeConfiguration -> [PackageName]
cpeConfigurationPackages = foldr ((++) . cpeTermsPackages) []

runCpeQueries ::
     b
  -> (b -> Payload Cpe "cpe" -> Bool)
  -> Configuration (Terms (Payload Cpe "cpe"))
  -> Configuration Bool
runCpeQueries x f c = runIdentity $ for c $ Identity . queryTerms x f

-- eitherDecodeStrict s :: Either [Char] (Configuration (Terms (Payload Cpe "cpe")))

collapse :: Configuration Bool -> Bool
collapse (Leaf x) = x
collapse (Branch op xs) =
  let ys = NE.map collapse xs
  in case op of
       And -> getAll . foldMap All $ ys
       Or -> getAny . foldMap Any $ ys

queryCpeConfiguration ::
     b
  -> (b -> Cpe -> Bool)
  -> Configuration (Terms (Payload Cpe "cpe"))
  -> Bool
queryCpeConfiguration x f =
  collapse . runCpeQueries x (\b (Payload cpe) -> f b cpe)

cpeConfigurationMatch ::
     (PackageName, PackageVersion) -> CpeConfiguration -> Bool
cpeConfigurationMatch pkg cfg =
  let fn =
        if ln == 1
          then cpeMatchExact
          else cpeMatchExact
      ln = foldr (\terms acc -> length terms + acc) 0 cfg
  in queryCpeConfiguration pkg (go fn) cfg
  where
    go fn pkg' cpe =
      case fn pkg' cpe of
        Just True -> True
        Just False -> False
        Nothing -> False
