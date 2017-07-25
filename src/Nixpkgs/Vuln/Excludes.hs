{-# LANGUAGE TemplateHaskell #-}

module Nixpkgs.Vuln.Excludes
  ( Excludes(..)
  , parseExcludes
  ) where

import Protolude


import Data.FileEmbed
import Data.Yaml

data Excludes = Excludes
  { nvdExcludes :: [Text]
  , glsaExcludes :: [Text]
  } deriving (Eq, Show)

instance FromJSON Excludes where
  parseJSON (Object o) =
    Excludes <$> (o .: "nvd" >>= (.: "excludes")) <*>
    (o .: "glsa" >>= (.: "excludes"))
  parseJSON _ = mzero

excludes :: ByteString
excludes = $(embedFile "data/excludes.yaml")

parseExcludes :: Excludes
parseExcludes =
  let excludesOrErr = decodeEither excludes
  in case excludesOrErr of
       Left e -> panic $ "Can't parse excludes: " <> toS e
       Right es -> es
