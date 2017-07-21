module Main where

import Protolude

import qualified Data.Vector as Vec
import           Distribution.Package

main :: IO ()
main = do
  pkgs <- parsePackages "/home/pbogdan/packages.json"
  print . Vec.head $ pkgs
