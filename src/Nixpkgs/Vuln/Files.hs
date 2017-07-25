module Nixpkgs.Vuln.Files
  (findFile) where

import Protolude

import Paths_nvs
import System.Directory hiding (findFile)


findFile :: FilePath -> IO FilePath
findFile path = do
  haveLocal <- doesFileExist path
  if haveLocal
    then return path
    else getDataFileName path
