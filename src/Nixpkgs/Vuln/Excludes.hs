module Nixpkgs.Vuln.Excludes
  ( Excludes(..)
  , parseExcludes
  ) where

import           Protolude

import qualified Data.ByteString as Bytes
import           Data.Yaml

data Excludes = Excludes
  { nvdExcludes :: [Text]
  , glsaExcludes :: [Text]
  } deriving (Eq, Show)

instance FromJSON Excludes where
  parseJSON (Object o) =
    Excludes <$> (o .: "nvd" >>= (.: "excludes")) <*>
    (o .: "glsa" >>= (.: "excludes"))
  parseJSON _ = mzero


parseExcludes :: FilePath -> IO Excludes
parseExcludes path = do
  s <- Bytes.readFile path
  let excludesOrErr = decodeEither s
  case excludesOrErr of
    Left e -> panic $ "Can't parse excludes: " <> toS e
    Right es -> return es
