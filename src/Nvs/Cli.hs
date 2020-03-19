{-|
Module      : Nvs.Cli
Description : Command line interface to nvs.
Copyright   : (c) Piotr Bogdan, 2017
License     : BSD3
Maintainer  : ppbogdan@gmail.com
Stability   : experimental
Portability : Unknown

Command line interface to nvs.

-}

{-# LANGUAGE FlexibleContexts #-}

module Nvs.Cli
  ( defaultMain
  )
where

import           Protolude               hiding ( option )

import           Control.Monad.Logger
import           Nvs.Cli.Opts
import           Nvs.Report
import           Nvs.Types
import           Options.Applicative            ( execParser )

-- | Default main function.
defaultMain :: IO ()
defaultMain = run =<< execParser
  (parseOptions `withInfo` "Experimental CVE scanner for nixpkgs")

run :: Opts -> IO ()
run opts =
  runStderrLoggingT
    $ filterLogger (\_ lvl -> optsVerbose opts || (lvl >= LevelWarn))
    $ do
        ret <- runExceptT $ do
          logInfoN "Generating report"
          report (map toS (optsNvdFeeds opts))
                 (toS . optsDerivation $ opts)
                 (optsOutput opts)
        case ret of
          Left (ShellCommandError _ msg) -> do
            logErrorN $ "Shell command failed: " <> show msg
            liftIO exitFailure
          Left (FileParseError path msg) -> do
            logErrorN $ "Failed parsing file " <> toS path <> ":" <> show msg
            liftIO exitFailure
          Right _ -> liftIO exitSuccess

