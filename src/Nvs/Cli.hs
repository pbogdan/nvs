{-# LANGUAGE FlexibleContexts #-}

module Nvs.Cli
  ( defaultMain
  )
where

import           Protolude               hiding ( option )

import           Control.Monad.Logger
import           Nvs.Cli.Opts
import           Nvs.Report
import           Options.Applicative            ( execParser )

-- | Default main function.
defaultMain :: IO ()
defaultMain = run =<< execParser
  (parseOptions `withInfo` "Experimental CVE scanner for nixpkgs")

run :: Opts -> IO ()
run opts =
  runStderrLoggingT
    $ filterLogger (\_ lvl -> optsVerbose opts || (lvl >= LevelWarn))
    $ report (map toS (optsNvdFeeds opts))
             (toS . optsDerivation $ opts)
             (optsOutput opts)
