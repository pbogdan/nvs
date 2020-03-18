module Nvd.Cve.Types
  ( Affects(..)
  )
where

import           Protolude

import           Nixpkgs.Packages.Types

{-
isAffected a (p, v) `elem` packages a
-}
class Affects a where
  packages :: a -> [PackageName]
  isAffected :: a -> (PackageName, PackageVersion) -> Bool
