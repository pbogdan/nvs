module Main where

import Protolude

import qualified Data.Vector as Vec
import           Distribution.Nixpkgs.Packages

main :: IO ()
main = do
  pkgs <- getPackages "/home/pbogdan/packages.json"
  print . Vec.head $ pkgs
