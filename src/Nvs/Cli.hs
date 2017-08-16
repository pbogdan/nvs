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

run :: Options -> IO ()
run (Options nvdFeed nixpkgs mode matching out verbose) =
  runStderrLoggingT $
  filterLogger (\_ lvl -> verbose || (lvl >= LevelWarn)) $
  withSystemTempDirectory "nvs" $ \tmpDir -> do
    ret <-
      runExceptT $ do
        logInfoN "Generating maintainers.json"
        generateMaintainers nixpkgs tmpDir `catchE`
          (throwE . flip ShellCommandError "Preparing maintainers.json failed")
        logInfoN "Generating packages.json"
        generatePackages nixpkgs tmpDir `catchE`
          (throwE . flip ShellCommandError "Preparing packages.json failed")
        logInfoN "Generating report"
        report
          (toS nvdFeed)
          (toS tmpDir <> "/packages.json")
          (toS tmpDir <> "/maintainers.json")
          (toS out)
          mode
          matching
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
      option
        "-E"
        ("let m = import " <> nixpkgs <>
         "/lib/maintainers.nix; in builtins.toJSON m")
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
