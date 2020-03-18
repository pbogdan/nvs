{-|
Module      : Nvs.Files
Copyright   : (c) Piotr Bogdan, 2017
License     : BSD3
Maintainer  : ppbogdan@gmail.com
Stability   : experimental
Portability : Unknown


-}
module Nvs.Files
  ( findFile
  )
where

import           Protolude

import           Paths_nvs
import           System.Directory        hiding ( findFile )

-- | Resolve a file path that's expected to be either relative to the current
-- working directory, or be a part of the package by being included in
-- data-files section of the cabal file.
--
-- This is used when locating files such as data/excludes.yaml.
findFile
  :: MonadIO m
  => FilePath -- ^ Path to be resolved
  -> m FilePath
findFile path = do
  haveLocal <- liftIO . doesFileExist $ path
  if haveLocal then return path else liftIO . getDataFileName $ path
