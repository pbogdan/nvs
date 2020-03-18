{-|
Module      : Nvs.Excludes
Description : Vulnerability exclusion support.
Copyright   : (c) Piotr Bogdan, 2017
License     : BSD3
Maintainer  : ppbogdan@gmail.com
Stability   : experimental
Portability : Unknown

Exclusion database support. The database store vulnerability identifier that
should be exluded when producing the report. 

The database is currently stored in YAML format in @data/vuln-excludes.yaml@
file. The format of the database is as follows:

@
  nvd:
    excludes:
      - CVE-2017-01
      - CVE-2017-02
  glsa:
    excludes:
      - GLSA-201707-01
      - GLSA-201707-02

@

-}

{-# LANGUAGE FlexibleContexts #-}

module Nvs.Excludes
  ( Excludes(..)
  , parseExcludes
  )
where

import           Protolude

import qualified Data.ByteString               as Bytes
import           Data.Yaml
import           Nvd.Cve
import           Nvs.Types

-- | Representation of entries in the excludes database.
data Excludes = Excludes
  { nvdExcludes :: [CveId] -- ^ Exclusions for vulnerabilites retrieved from
                          -- NVD. Those should be CVE IDs.
  , glsaExcludes :: [Text] -- ^ Exclusionos for vulnerabilites retrieved from
                           -- GLSA repository. Those should be GLSA IDs.
  } deriving (Eq, Show)

instance FromJSON Excludes where
  parseJSON (Object o) =
    Excludes
      <$> (o .: "nvd" >>= (.: "excludes"))
      <*> (o .: "glsa" >>= (.: "excludes"))
  parseJSON _ = mzero

-- | Load and parse excludes database.
parseExcludes
  :: (MonadError NvsError m, MonadIO m)
  => FilePath -- ^ path tot he excludes database
  -> m Excludes
parseExcludes path = do
  s <- liftIO . Bytes.readFile $ path
  let excludesOrErr = first displayException . decodeEither' $ s
  case excludesOrErr of
    Left  e  -> throwError $ FileParseError path (toS e)
    Right es -> return es
