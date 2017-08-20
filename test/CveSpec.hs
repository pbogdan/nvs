{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module CveSpec where

import           Protolude hiding (Any)

import qualified Data.HashMap.Strict as HashMap
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import           Data.String
import qualified Data.Text as Text
import qualified Data.Vector as Vec
import           GHC.TypeLits
import           Nixpkgs.Packages
import           Nixpkgs.Packages.Aliases
import           Nixpkgs.Packages.Types
import           Nvd.Cpe
import           Nvd.Cpe.Configuration
import           Nvd.Cpe.Uri
import           Nvd.Cve
import           Test.Hspec
import           Test.Hspec.QuickCheck (modifyMaxSize)
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

instance Arbitrary PackageName where
  arbitrary = PackageName <$> (arbitrary `suchThat` (\x -> Text.length x <= 10))

instance Arbitrary PackageVersion where
  arbitrary = do
    major <- show <$> elements [1 .. 10 :: Int] :: Gen String
    minor <- show <$> elements [1 .. 10 :: Int]
    patch <- show <$> elements [1 .. 10 :: Int]
    return . PackageVersion . toS $ (major <> "." <> minor <> "." <> patch)

instance Arbitrary CveId where
  arbitrary = do
    year <- show <$> elements [2000 .. 2020 :: Int] :: Gen String
    id <- show <$> elements [0 .. 10000 :: Int]
    return $ fromString ("CVE-" <> year <> "-" <> id)

instance Arbitrary CpePart where
  arbitrary = elements [Application, OS, Hardware, Unknown]

instance Arbitrary b => Arbitrary (CpeValue a b) where
  arbitrary = oneof [pure Any, pure NA, CpeValue <$> arbitrary]

instance Arbitrary CpeUri where
  arbitrary =
    CpeUri <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary

instance Arbitrary Cpe where
  arbitrary = Cpe <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Op where
  arbitrary = elements [And, Or]

instance Arbitrary a => Arbitrary (Terms a) where
  arbitrary = Terms <$> arbitrary <*> arbitrary

instance (s ~ PayloadKey a, KnownSymbol s, Arbitrary a) =>
         Arbitrary (Payload a s) where
  arbitrary = Payload <$> arbitrary

instance Arbitrary a => Arbitrary (Configuration a) where
  arbitrary = oneof [Leaf <$> arbitrary, Branch <$> arbitrary <*> arbitrary]

instance Arbitrary VendorProduct where
  arbitrary = VendorProduct <$> arbitrary <*> arbitrary

instance Arbitrary VendorData where
  arbitrary =
    VendorData <$> (arbitrary `suchThat` (\x -> Text.length x <= 10)) <*>
    arbitrary

instance Arbitrary (Cve VendorData) where
  arbitrary =
    Cve <$> arbitrary <*> (arbitrary `suchThat` (\x -> Text.length x <= 10)) <*>
    arbitrary

newtype CveConstProducts = CveConstProducts
  { unCveConstProducts :: Cve VendorData
  } deriving (Eq, Show)

dummyPackage :: (PackageName, PackageVersion)
dummyPackage = ("dummy", "0.0.1")

dummyVendorData :: VendorData
dummyVendorData = VendorData "" [VendorProduct "dummy" ["0.0.1"]]

instance Arbitrary CveConstProducts where
  arbitrary = do
    cve <- Cve <$> arbitrary <*> pure "description" <*> pure [dummyVendorData]
    return . CveConstProducts $ cve

newtype CveMultiple = CveMultiple
  { unCveMultiple :: Cve VendorData
  } deriving (Eq, Show)

instance Arbitrary CveMultiple where
  arbitrary =
    CveMultiple <$> (arbitrary `suchThat` (\x -> (length . cveAffects $ x) > 1))

instance Arbitrary Package where
  arbitrary =
    Package <$> pure "x86_64" <*> arbitrary <*> arbitrary <*>
    (arbitrary `suchThat` (\x -> Text.length x < 10)) <*>
    pure undefined

{-# ANN spec ("HLint: ignore Redundant do" :: Text) #-}
spec :: Spec
spec = do
  describe "Given CVE affecting multiple packages" $ do
    modifyMaxSize (const 100) $ do
      it "all of them are reported" $
        property $ \(CveMultiple cve) ->
          let pkgs = cveProducts cve
              aliases = HashMap.empty
              cves =
                foldl'
                  (\acc (name, _) -> HashMap.insert name (Set.singleton cve) acc)
                  HashMap.empty
                  pkgs
              ret =
                map
                  (\(name, ver) ->
                     cvesForPackage
                       (Package "" name ver "" undefined)
                       aliases
                       cves)
                  pkgs
          in length pkgs `shouldBe` length ret
  describe "Given multiple CVEs affecting single product" $ do
    modifyMaxSize (const 100) $ do
      it "all of them are reported" $
        property $ \(cves :: NonEmpty CveConstProducts) ->
          let byPackage =
                cvesByPackage
                  (Vec.fromList . NE.toList . NE.nub . NE.map unCveConstProducts $
                   cves)
              forPackage =
                cvesForPackage
                  (Package "" "dummy" "0.0.1" "" undefined)
                  HashMap.empty
                  byPackage
          in (length . NE.nub $ cves) `shouldBe` (Set.size . snd $ forPackage)
  describe "Given non-empty aliases database" $ do
    modifyMaxSize (const 100) $ do
      it "they are taken into account" $
        property $ \(cve :: Cve VendorData) -> do
          alias <-
            arbitrary `suchThat` (\x -> x `notElem` map fst (cveProducts cve))
          let pNames = map fst . cveProducts $ cve
              aliases =
                HashMap.fromList $
                zip pNames (map (flip PackageAlias [alias]) pNames)
              packages = cveProducts cve
              cves =
                foldl'
                  (\acc (_, _) -> HashMap.insert alias (Set.singleton cve) acc)
                  HashMap.empty
                  packages
              ret =
                map
                  (\(name, ver) ->
                     cvesForPackage
                       (Package "" name ver "" undefined)
                       aliases
                       cves)
                  packages
          return (length packages `shouldBe` length ret)
