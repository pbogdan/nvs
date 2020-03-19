{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nvd.Cpe.Configuration
  ( Op(..)
  , Terms(..)
  , Configuration(..)
  , CpeConfiguration
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

data Op
  = And
  | Or
  deriving (Eq, Generic, Ord, Show)

instance FromJSON Op where
  parseJSON (String "AND") = pure And
  parseJSON (String "OR" ) = pure Or
  parseJSON x              = typeMismatch "Op" x

instance ToJSON Op where
  toJSON And = String "AND"
  toJSON Or  = String "OR"

data Terms a =
  Terms Op [a]
  deriving (Eq, Functor, Foldable, Generic, Traversable, Ord, Show)

queryTerms :: b -> (b -> a -> Bool) -> Terms a -> Bool
queryTerms y f (Terms op xs) =
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
  | Branch Op
           (NonEmpty (Configuration a))
  deriving (Eq, Generic, Foldable, Traversable, Functor, Ord, Show)

instance FromJSON a => FromJSON (Configuration a) where
  parseJSON js@(Object o) = do
    mChildren <- o .:? "children"
    mOp       <- o .:? "operator"
    case (mChildren, mOp) of
      (Nothing, Just _ ) -> Leaf <$> parseJSON js
      (Just cs, Just op) -> Branch <$> pure op <*> parseJSON cs
      (Just _ , Nothing) -> typeMismatch "Configration" js
      (Nothing, Nothing) -> typeMismatch "Configration" js
  parseJSON x = typeMismatch "Configuration" x

instance ToJSON a => ToJSON (Configuration a) where

type CpeTerms = Terms Cpe

cpeTermsPackages :: CpeTerms -> [PackageName]
cpeTermsPackages = catMaybes . foldr go []
 where
  go :: Cpe -> [Maybe PackageName] -> [Maybe PackageName]
  go cpe acc = (cpeUriPackageName . cpeCpeUri $ cpe) : acc

type CpeConfiguration = Configuration CpeTerms

instance Affects CpeConfiguration where
  packages   = foldMap cpeTermsPackages
  isAffected = flip cpeConfigurationMatch

runCpeQueries
  :: b -> (b -> Cpe -> Bool) -> CpeConfiguration -> Configuration Bool
runCpeQueries x f c = runIdentity $ for c $ Identity . queryTerms x f

collapse :: Configuration Bool -> Bool
collapse (Leaf x) = x
collapse (Branch op xs) =
  let ys = NE.map collapse xs
  in  case op of
        And -> getAll . foldMap All $ ys
        Or  -> getAny . foldMap Any $ ys

queryCpeConfiguration :: b -> (b -> Cpe -> Bool) -> CpeConfiguration -> Bool
queryCpeConfiguration x f = collapse . runCpeQueries x f

cpeConfigurationMatch
  :: (PackageName, PackageVersion) -> CpeConfiguration -> Bool
cpeConfigurationMatch pkg cfg =
  let fn = if ln == 1 then cpeMatchExact else cpeMatchExact
      ln = foldr (\terms acc -> length terms + acc) 0 cfg
  in  if isReleaseSeries cfg
        then seriesMatch pkg cfg
        else queryCpeConfiguration pkg (go fn) cfg
 where
  go fn pkg' cpe = case fn pkg' cpe of
    Just True  -> True
    Just False -> False
    Nothing    -> False

isReleaseSeries :: CpeConfiguration -> Bool
isReleaseSeries = getAll . foldMap goCfg
 where
  goCfg :: CpeTerms -> All
  goCfg terms = foldMap goTerms terms <> All (sameNames terms) <> All
    ((length . allNames $ terms) > 1)
  goTerms :: Cpe -> All
  goTerms cpe =
    let previousVersions = fromMaybe False (cpePreviousVersions cpe)
    in  All previousVersions

allNames :: CpeTerms -> [PackageName]
allNames =
  catMaybes
    . foldr (\cpe acc -> (cpeUriPackageName . cpeCpeUri $ cpe) : acc) mempty

sameNames :: CpeTerms -> Bool
sameNames terms = (length . nub $ allNames terms) == 1

allVersions :: CpeTerms -> [PackageVersion]
allVersions =
  catMaybes
    . foldr (\cpe acc -> (cpeUriPackageVersion . cpeCpeUri $ cpe) : acc) mempty

seriesMatch :: (PackageName, PackageVersion) -> CpeConfiguration -> Bool
seriesMatch pkg@(_, version) cfg =
  let vs        = foldMap allVersions cfg
      candidate = versionCandidate version vs
  in  case candidate of
        Nothing -> False
        Just v  -> queryCpeConfiguration pkg (go v) cfg
 where
  go :: PackageVersion -> (PackageName, PackageVersion) -> Cpe -> Bool
  go v pkg' cpe =
    let ret = do
          cVersion <- cpeUriPackageVersion . cpeCpeUri $ cpe
          if cVersion == v then cpeMatch pkg' cpe else Just False
    in  fromMaybe False ret
