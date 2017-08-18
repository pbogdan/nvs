{-|
Module      : Nvs.Cli.Opts
Description : Command line arguments parser for nvs command line interface.
Copyright   : (c) Piotr Bogdan, 2017
License     : BSD3
Maintainer  : ppbogdan@gmail.com
Stability   : experimental
Portability : Unknown

Command line arguments parser for nvs command line interface.

-}

module Nvs.Cli.Opts
  ( Opts(..)
  , parseOptions
  , withInfo
  ) where

import Protolude

import Nvs.Report
import Options.Applicative

-- | Command line options for the CLI.
data Opts = Opts
  { optsNvdFeeds :: [Text]
  , optsNixpkgs :: Text
  , optsOutput :: Output
  , optsMatching :: Matching
  , optsOutPath :: Text
  , optsVerbose :: Bool
  } deriving (Eq, Show)

-- | Parser for the command line options.
parseOptions :: Parser Opts
parseOptions =
  Opts <$>
  some
    (toS <$>
     strOption
       (long "nvd-feed" <> metavar "nvd-feed" <>
        help
          "Path to a copy of the NVD JSON feed. May be specified multiple times.")) <*>
  (toS <$>
   strOption
     (long "nixpkgs" <> metavar "nixpkgs" <> help "Path to nixpkgs checkout.")) <*>
  flag
    HTML
    Markdown
    (long "markdown" <> help "render markdown instead of HTML.") <*>
  flag Simple Cpe (long "cpe" <> help "use CPE matching mode.") <*>
  (toS <$>
   argument str (metavar "file" <> help "Output path for the generated report.")) <*>
  switch (long "verbose" <> help "Verbose output.")

-- | Convenience function to add @--help@ support given a parser and
-- description.
withInfo :: Parser a -> Text -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc (toS desc)
