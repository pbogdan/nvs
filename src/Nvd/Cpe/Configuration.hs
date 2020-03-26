{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nvd.Cpe.Configuration
  ( Terms(..)
  , Configuration(..)
  , match
  , collectNames
  )
where

import           Protolude

import           Control.Arrow                  ( (>>>) )
import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                , Value(..)
                                                , object
                                                , (.=)
                                                , (.:)
                                                , (.:?)
                                                )
import           Data.Aeson.Types               ( typeMismatch )
import           Data.List                      ( nub )
import           Nixpkgs.Packages.Types         ( PackageName
                                                , PackageVersion
                                                )
import           Nixpkgs.Packages.Versions      ( versionCandidate )
import           Nvd.Cpe                        ( Cpe(..)
                                                , cpeMatch
                                                , cpeMatchExact
                                                )
import           Nvd.Cpe.Uri                    ( cpeUriPackageName
                                                , cpeUriPackageVersion
                                                )

data Operator
  = And
  | Or
  deriving (Eq, Generic, Ord, Show)

instance FromJSON Operator where
  parseJSON (String "AND") = pure And
  parseJSON (String "OR" ) = pure Or
  parseJSON x              = typeMismatch "Operator" x

instance ToJSON Operator where
  toJSON And = String "AND"
  toJSON Or  = String "OR"

data Terms a =
  Terms Operator [a]
  deriving (Eq, Functor, Foldable, Generic, Traversable, Ord, Show)

instance FromJSON a => FromJSON (Terms a) where
  parseJSON (Object o) =
    Terms
      <$> o
      .:  "operator"
      <*> (o .:? "cpe_match" >>= \x -> do
            y <- sequenceA $ parseJSON <$> x
            return . fromMaybe [] $ y
          )
  parseJSON x = typeMismatch "Terms" x

instance ToJSON a => ToJSON (Terms a) where
  toJSON (Terms op xs) = object ["operator" .= op, "matches" .= toJSON xs]

data Configuration a
  = Leaf (Terms a)
  | Branch (Terms
           (Configuration a))
  deriving (Eq, Generic, Foldable, Traversable, Functor, Ord, Show)

instance FromJSON a => FromJSON (Configuration a) where
  parseJSON js@(Object o) = do
    mOperator <- o .:? "operator"
    mChildren <- o .:? "children"
    case (mOperator, mChildren) of
      (Just _ , Nothing) -> Leaf <$> parseJSON js
      (Just op, Just cs) -> Branch <$> (Terms <$> pure op <*> parseJSON cs)
      (Nothing, Just _ ) -> typeMismatch "Configuration" js
      (Nothing, Nothing) -> typeMismatch "Configuration" js
  parseJSON x = typeMismatch "Configuration" x

instance ToJSON a => ToJSON (Configuration a) where

collapse :: Configuration Bool -> Bool
collapse (Leaf (Terms op xs)) = case op of
  And -> getAll . foldMap All $ xs
  Or  -> getAny . foldMap Any $ xs
collapse (Branch (Terms op xs)) =
  let ys = map collapse xs in collapse (Leaf (Terms op ys))

query :: (b -> a -> Bool) -> b -> Configuration a -> Bool
query f x = collapse . fmap (f x)

match :: (PackageName, PackageVersion) -> Configuration Cpe -> Bool
match pkg cfg =
  let fn = if length cfg == 1 then cpeMatchExact else cpeMatchExact
  in  if isReleaseSeries cfg
        then releaseSeriesMatch pkg cfg
        else query (go fn) pkg cfg
 where
  go fn pkg' cpe = case fn pkg' cpe of
    Just True  -> True
    Just False -> False
    Nothing    -> False

isReleaseSeries :: Configuration Cpe -> Bool
isReleaseSeries cfg =
  getAll
    . mconcat
    $ [ foldMap goTerms cfg
      , All (sameNames cfg)
      , All ((length . collectNames $ cfg) > 1)
      ]
 where
  goTerms :: Cpe -> All
  goTerms cpe =
    let previousVersions = fromMaybe False (cpePreviousVersions cpe)
    in  All previousVersions

collectNames :: Configuration Cpe -> [PackageName]
collectNames =
  catMaybes . foldr (cpeCpeUri >>> cpeUriPackageName >>> (:)) mempty

collectVersions :: Configuration Cpe -> [PackageVersion]
collectVersions =
  catMaybes . foldr ((:) . cpeUriPackageVersion . cpeCpeUri) mempty

sameNames :: Configuration Cpe -> Bool
sameNames = collectNames >>> nub >>> length >>> (== 1)

releaseSeriesMatch :: (PackageName, PackageVersion) -> Configuration Cpe -> Bool
releaseSeriesMatch pkg@(_, version) cfg =
  let vs        = collectVersions cfg
      candidate = versionCandidate version vs
  in  case candidate of
        Nothing -> False
        Just v  -> query (go v) pkg cfg
 where
  go :: PackageVersion -> (PackageName, PackageVersion) -> Cpe -> Bool
  go v pkg' cpe =
    let ret = do
          cVersion <- cpeUriPackageVersion . cpeCpeUri $ cpe
          if cVersion == v then cpeMatch pkg' cpe else Just False
    in  fromMaybe False ret
