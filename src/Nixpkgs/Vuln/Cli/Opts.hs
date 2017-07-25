{-|
Module      : Nixpkgs.Vuln.Cli.Opts
Description : Command line arguments parser for nvs command line interface.
Copyright   : (c) Piotr Bogdan, 2017
License     : BSD3
Maintainer  : ppbogdan@gmail.com
Stability   : experimental
Portability : Unknown

Command line arguments parser for nvs command line interface.

-}

module Nixpkgs.Vuln.Cli.Opts
  ( Options(..)
  , parseOptions
  , withInfo
  ) where

import Protolude

import Nixpkgs.Vuln.Report
import Options.Applicative

-- | Command line options for the CLI. The fields represent:
-- - path to NVD JSON feed file
-- - path to nixpkgs checkout
-- - rendering mode which specifies output format
-- - output path for generated report
data Options =
  Options Text
          Text
          RenderMode
          Text
  deriving (Eq, Show)

-- | Parser for the command line options.
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
  flag HTML Markdown (long "markdown" <> help "render markdown instead of HTML") <*>
  (toS <$>
   argument str (metavar "file" <> help "Output path for the generated report"))

-- | Convenience function to add @--help@ support given a parser and
-- description.
withInfo :: Parser a -> Text -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc (toS desc)
