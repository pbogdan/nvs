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
  ) where

import           Protolude hiding (msg)

import           Control.Monad.Logger
import           Control.Monad.Trans.Except
import qualified Data.Text.IO as Text
import           Nvs.Cli.Opts
import           Nvs.Report
import           Nvs.Types
import           Options.Applicative (execParser)
import           Shell
import           System.IO.Temp
import           Text.Read (read)

-- | Default main function.
defaultMain :: IO ()
defaultMain =
  run =<<
  execParser (parseOptions `withInfo` "Experimental CVE scanner for nixpkgs")

run :: Opts -> IO ()
run opts =
  runStderrLoggingT $
  filterLogger (\_ lvl -> optsVerbose opts || (lvl >= LevelWarn)) $
  withSystemTempDirectory "nvs" $ \tmpDir -> do
    ret <-
      runExceptT $ do
        logInfoN "Generating maintainers.json"
        generateMaintainers (optsNixpkgs opts) tmpDir `catchE`
          (throwE . flip ShellCommandError "Preparing maintainers.json failed")
        logInfoN "Generating packages.json"
        generatePackages (optsNixpkgs opts) tmpDir `catchE`
          (throwE . flip ShellCommandError "Preparing packages.json failed")
        logInfoN "Generating report"
        report
          (map toS (optsNvdFeeds opts))
          (toS tmpDir <> "/packages.json")
          (toS tmpDir <> "/maintainers.json")
          (toS (optsOutPath opts))
          (optsOutput opts)
          (optsMatching opts)
    case ret of
      Left (ShellCommandError _ msg) -> do
        logErrorN $ "Shell command failed: " <> show msg
        liftIO exitFailure
      Left (FileParseError path msg) -> do
        logErrorN $ "Failed parsing file " <> toS path <> ":" <> show msg
        liftIO exitFailure
      Right _ -> liftIO exitSuccess

generateMaintainers ::
     (MonadIO m, MonadLogger m, MonadError ExitCode m)
  => Text
  -> FilePath
  -> m ()
generateMaintainers nixpkgs tmpDir = do
  ret <-
    shell "nix-instantiate" $ do
      arg "--eval"
      option "-I" ("nixpkgs=" <> nixpkgs)
      option
        "-E"
        "with (import <nixpkgs> {}); builtins.toJSON pkgs.lib.maintainers"
      raw "2>/dev/null"
  liftIO $ Text.writeFile (tmpDir <> "/maintainers.json") (read . toS $ ret)

generatePackages ::
     (MonadIO m, MonadLogger m, MonadError ExitCode m)
  => Text
  -> FilePath
  -> m ()
generatePackages nixpkgs tmpDir =
  shell_ "nix-env" $ do
    switch "--arg"
    raw "config '{}'"
    switch "-qaP"
    switch "--json"
    arg "*"
    option "-f" nixpkgs
    raw $ " > " <> toS tmpDir <> "/packages.json"
