{-# LANGUAGE QuasiQuotes #-}

module PackagesSpec where

import           Protolude

import           Data.Aeson
import           Data.Aeson.QQ
import           Nixpkgs.Packages
import           Test.Hspec

{-# ANN spec ("HLint: ignore Redundant do" :: Text) #-}
spec :: Spec
spec = do
  describe "When parsing packages" $ do
    it "a simple value is parsed"
      $          fromJSON packagesJson
      `shouldBe` Success packagesJson
    it "packages with multiple versions are preserved"
      $          length
      <$>        (fromJSON packagesJson :: Result PackageSet)
      `shouldBe` Success 2

packagesJson :: Value
packagesJson = [aesonQQ|
{
   "nixos.mysql55" : {
      "system" : "x86_64-linux",
      "meta" : {
         "position" : "/home/pbogdan/nixpkgs/pkgs/servers/sql/mysql/5.7.x.nix:61",
         "platforms" : [
            "i686-linux",
            "x86_64-linux",
            "armv5tel-linux",
            "armv6l-linux",
            "armv7l-linux",
            "aarch64-linux",
            "mips64el-linux",
            "x86_64-darwin",
            "i686-freebsd",
            "x86_64-freebsd",
            "i686-openbsd",
            "x86_64-openbsd",
            "i686-netbsd",
            "x86_64-netbsd",
            "x86_64-solaris"
         ],
         "description" : "The world's most popular open source database",
         "outputsToInstall" : [
            "out"
         ],
         "homepage" : "http://www.mysql.com/"
      },
      "name" : "mysql-5.5.0"
   },
   "nixos.mysql57" : {
      "system" : "x86_64-linux",
      "meta" : {
         "position" : "/home/pbogdan/nixpkgs/pkgs/servers/sql/mysql/5.7.x.nix:61",
         "platforms" : [
            "i686-linux",
            "x86_64-linux",
            "armv5tel-linux",
            "armv6l-linux",
            "armv7l-linux",
            "aarch64-linux",
            "mips64el-linux",
            "x86_64-darwin",
            "i686-freebsd",
            "x86_64-freebsd",
            "i686-openbsd",
            "x86_64-openbsd",
            "i686-netbsd",
            "x86_64-netbsd",
            "x86_64-solaris"
         ],
         "description" : "The world's most popular open source database",
         "outputsToInstall" : [
            "out"
         ],
         "homepage" : "http://www.mysql.com/"
      },
      "name" : "mysql-5.7.0"
   }

}
|]
