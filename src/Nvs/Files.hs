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
findFile :: MonadIO m => FilePath -> m FilePath
findFile path = do
  haveLocal <- liftIO . doesFileExist $ path
  if haveLocal then return path else liftIO . getDataFileName $ path
