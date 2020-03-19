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
  )
where

import           Protolude

import           Nvs.Report
import           Options.Applicative

-- | Command line options for the CLI.
data Opts = Opts
  { optsOutput :: Output
  , optsVerbose :: Bool
  , optsNvdFeeds :: [Text]
  , optsDerivation :: Text
  } deriving (Eq, Show)

-- | Parser for the command line options.
parseOptions :: Parser Opts
parseOptions =
  Opts
    <$> asum
          [ flag' HTML     (long "html" <> help "Render HTML.")
          , flag' Markdown (long "markdown" <> help "Render Markdown.")
          , flag' JSON     (long "json" <> help "Render JSON.")
          ]
    <*> switch (long "verbose" <> help "Verbose output.")
    <*> some
          (strOption
            (  long "nvd-feed"
            <> metavar "nvd-feed"
            <> help
                 "Path to a copy of the NVD JSON feed. May be specified multiple times."
            )
          )
    <*> argument str (metavar "derivation")

-- | Convenience function to add @--help@ support given a parser and
-- description.
withInfo :: Parser a -> Text -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc (toS desc)
