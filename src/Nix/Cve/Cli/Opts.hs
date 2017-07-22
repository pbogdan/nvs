module Nix.Cve.Cli.Opts
  ( Options(..)
  , parseOptions
  , withInfo
  ) where

import Protolude

import Options.Applicative

data Options =
  -- | The fields specify path to NVD JSON feed, path to nixpkgs checkout, and
  -- the output path for the generated report.
  Options Text
          Text
          Text
  deriving (Eq, Show)

parseOptions :: Parser Options
parseOptions =
  Options <$>
  (toS <$>
   strOption
     (long "nvd-feed" <> metavar "nvd-feed" <>
      help "Path to a copy of the NVD JSON feed")) <*>
  (toS <$>
   strOption
     (long "nixpkgs" <> metavar "nixpkgs" <> help "Path to nixpkgs checkout")) <*>
  (toS <$>
   argument str (metavar "file" <> help "Output path for the generated report"))

withInfo :: Parser a -> Text -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc (toS desc)
