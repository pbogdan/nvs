{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nvd.Cpe.Configuration
  ( Operand(..)
  , Terms(..)
  , Configuration(..)
  )
where

import           Protolude

import           Data.Aeson
import           Data.Aeson.Types               ( typeMismatch )
import           Data.List                      ( nub )
import qualified Data.List.NonEmpty            as NE
import           Nixpkgs.Packages.Types
import           Nixpkgs.Packages.Versions
import           Nvd.Cpe
import           Nvd.Cpe.Uri             hiding ( Any )
import           Nvd.Cve.Types

data Operand
  = And
  | Or
  deriving (Eq, Generic, Ord, Show)

instance FromJSON Operand where
  parseJSON (String "AND") = pure And
  parseJSON (String "OR" ) = pure Or
  parseJSON x              = typeMismatch "Operand" x

instance ToJSON Operand where
  toJSON And = String "AND"
  toJSON Or  = String "OR"

data Terms a =
  Terms Operand [a]
  deriving (Eq, Functor, Foldable, Generic, Traversable, Ord, Show)

queryTerms :: (b -> a -> Bool) -> b -> Terms a -> Bool
queryTerms f y (Terms op xs) =
  let ys = map (f y) xs
  in  case op of
        And -> getAll . foldMap All $ ys
        Or  -> getAny . foldMap Any $ ys

instance FromJSON (Terms Cpe) where
  parseJSON (Object o) =
    Terms
      <$> o
      .:  "operator"
      <*> (o .:? "cpe_match" >>= \x -> do
            y <- sequenceA $ parseJSON <$> x
            return . fromMaybe [] $ y
          )
  parseJSON x = typeMismatch "Terms" x

instance ToJSON (Terms Cpe) where
  toJSON (Terms op xs) = object ["op" .= op, "matches" .= toJSON xs]

data Configuration a
  = Leaf a
  | Branch Operand
           (NonEmpty (Configuration a))
  deriving (Eq, Generic, Foldable, Traversable, Functor, Ord, Show)

instance FromJSON a => FromJSON (Configuration a) where
  parseJSON js@(Object o) = do
    mChildren <- o .:? "children"
    mOperand  <- o .:? "operator"
    case (mChildren, mOperand) of
      (Nothing, Just _ ) -> Leaf <$> parseJSON js
      (Just cs, Just op) -> Branch <$> pure op <*> parseJSON cs
      (Just _ , Nothing) -> typeMismatch "Configuration" js
      (Nothing, Nothing) -> typeMismatch "Configuration" js
  parseJSON x = typeMismatch "Configuration" x

instance ToJSON a => ToJSON (Configuration a) where

cpeTermsPackages :: Terms Cpe -> [PackageName]
cpeTermsPackages = catMaybes . foldr go []
 where
  go :: Cpe -> [Maybe PackageName] -> [Maybe PackageName]
  go cpe acc = (cpeUriPackageName . cpeCpeUri $ cpe) : acc

instance Affects (Configuration (Terms Cpe)) where
  packages   = foldMap cpeTermsPackages
  isAffected = flip cpeConfigurationMatch

runCpeQueries
  :: (b -> Cpe -> Bool) -> b -> Configuration (Terms Cpe) -> Configuration Bool
runCpeQueries f x c = runIdentity $ for c $ Identity . queryTerms f x

collapse :: Configuration Bool -> Bool
collapse (Leaf x) = x
collapse (Branch op xs) =
  let ys = NE.map collapse xs
  in  case op of
        And -> getAll . foldMap All $ ys
        Or  -> getAny . foldMap Any $ ys

queryCpeConfiguration
  :: (b -> Cpe -> Bool) -> b -> Configuration (Terms Cpe) -> Bool
queryCpeConfiguration f x = collapse . runCpeQueries f x

cpeConfigurationMatch
  :: (PackageName, PackageVersion) -> Configuration (Terms Cpe) -> Bool
cpeConfigurationMatch pkg cfg =
  let fn = if ln == 1 then cpeMatchExact else cpeMatchExact
      ln = foldr (\terms acc -> length terms + acc) 0 cfg
  in  if isReleaseSeries cfg
        then seriesMatch pkg cfg
        else queryCpeConfiguration (go fn) pkg cfg
 where
  go fn pkg' cpe = case fn pkg' cpe of
    Just True  -> True
    Just False -> False
    Nothing    -> False

isReleaseSeries :: Configuration (Terms Cpe) -> Bool
isReleaseSeries = getAll . foldMap goCfg
 where
  goCfg :: Terms Cpe -> All
  goCfg terms = foldMap goTerms terms <> All (sameNames terms) <> All
    ((length . allNames $ terms) > 1)
  goTerms :: Cpe -> All
  goTerms cpe =
    let previousVersions = fromMaybe False (cpePreviousVersions cpe)
    in  All previousVersions

allNames :: Terms Cpe -> [PackageName]
allNames =
  catMaybes
    . foldr (\cpe acc -> (cpeUriPackageName . cpeCpeUri $ cpe) : acc) mempty

sameNames :: Terms Cpe -> Bool
sameNames terms = (length . nub $ allNames terms) == 1

allVersions :: Terms Cpe -> [PackageVersion]
allVersions =
  catMaybes
    . foldr (\cpe acc -> (cpeUriPackageVersion . cpeCpeUri $ cpe) : acc) mempty

seriesMatch
  :: (PackageName, PackageVersion) -> Configuration (Terms Cpe) -> Bool
seriesMatch pkg@(_, version) cfg =
  let vs        = foldMap allVersions cfg
      candidate = versionCandidate version vs
  in  case candidate of
        Nothing -> False
        Just v  -> queryCpeConfiguration (go v) pkg cfg
 where
  go :: PackageVersion -> (PackageName, PackageVersion) -> Cpe -> Bool
  go v pkg' cpe =
    let ret = do
          cVersion <- cpeUriPackageVersion . cpeCpeUri $ cpe
          if cVersion == v then cpeMatch pkg' cpe else Just False
    in  fromMaybe False ret
