{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module CveSpec where

import           Protolude

import qualified Data.HashMap.Strict as HashMap
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import           Data.String
import qualified Data.Text as Text
import qualified Data.Vector as Vec
import           Nixpkgs.Packages
import           Nixpkgs.Packages.Aliases
import           Nixpkgs.Packages.Types
import           Nvd.Cve
import           Test.Hspec
import           Test.Hspec.QuickCheck (modifyMaxSize)
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

instance Arbitrary PackageName where
  arbitrary = fromString <$> (arbitrary `suchThat` (\x ->length x <= 10))

instance Arbitrary PackageVersion where
  arbitrary = do
    major <- show <$> elements [1 .. 10 :: Int] :: Gen String
    minor <- show <$> elements [1 .. 10 :: Int]
    patch <- show <$> elements [1 .. 10 :: Int]
    return $ fromString (major <> "." <> minor <> "." <> patch)

instance Arbitrary CveId where
  arbitrary = do
    year <- show <$> elements [2000 .. 2020 :: Int] :: Gen String
    id <- show <$> elements [0 .. 10000 :: Int]
    return $ fromString ("CVE-" <> year <> "-" <> id)

instance Arbitrary Cve where
  arbitrary =
    Cve <$> arbitrary <*> arbitrary <*>
    (arbitrary `suchThat` (\x -> Text.length x <= 10))

newtype CveConstProducts = CveConstProducts
  { unCveConstProducts :: Cve
  } deriving (Eq, Show)

dummyPackage :: (PackageName, PackageVersion)
dummyPackage = ("dummy", "0.0.1")

instance Arbitrary CveConstProducts where
  arbitrary = do
    cve <- Cve <$> arbitrary <*> pure [dummyPackage] <*> pure "description"
    return . CveConstProducts $ cve

newtype CveMultiple = CveMultiple
  { unCveMultiple :: Cve
  } deriving (Eq, Show)

instance Arbitrary CveMultiple where
  arbitrary =
    CveMultiple <$> (arbitrary `suchThat` (\x -> (length . cveAffects $ x) > 1))

instance Arbitrary Package where
  arbitrary =
    Package <$> pure "x86_64" <*> arbitrary <*> arbitrary <*> pure undefined

{-# ANN spec ("HLint: ignore Redundant do" :: Text) #-}
spec :: Spec
spec = do
  describe "Given CVE affecting multiple packages" $ do
    modifyMaxSize (const 100000) $ do
      it "all of them are reported" $
        property $ \(CveMultiple cve) ->
          let packages = cveAffects cve
              aliases = HashMap.empty
              cves =
                foldl'
                  (\acc pkg -> HashMap.insert pkg (Set.singleton cve) acc)
                  HashMap.empty
                  packages
              ret =
                map
                  (\(name, ver) ->
                     cvesForPackage (Package "" name ver undefined) aliases cves)
                  packages
          in length packages `shouldBe` length ret
  describe "Given multiple CVEs affecting single product" $ do
    modifyMaxSize (const 1000) $ do
      it "all of them are reported" $
        property $ \(cves :: NonEmpty CveConstProducts) ->
          let byPackage =
                cvesByPackage
                  (Vec.fromList . NE.toList . NE.nub . NE.map unCveConstProducts $
                   cves)
              forPackage =
                cvesForPackage
                  (Package "" "dummy" "0.0.1" undefined)
                  HashMap.empty
                  byPackage
          in (length . NE.nub $ cves) `shouldBe` (Set.size . snd $ forPackage)
  describe "Given non-empty aliases database" $ do
    modifyMaxSize (const 100000) $ do
      it "they are taken into account" $
        property $ \(cve :: Cve) -> do
          alias <-
            arbitrary `suchThat` (\x -> x `notElem` map fst (cveAffects cve))
          let pNames = map fst . cveAffects $ cve
              aliases =
                HashMap.fromList $
                zip pNames (map (flip PackageAlias [alias]) pNames)
              packages = cveAffects cve
              cves =
                foldl'
                  (\acc (_, ver) ->
                     HashMap.insert (alias, ver) (Set.singleton cve) acc)
                  HashMap.empty
                  packages
              ret =
                map
                  (\(name, ver) ->
                     cvesForPackage (Package "" name ver undefined) aliases cves)
                  packages
          return (length packages `shouldBe` length ret)
