{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Nixpkgs.Vuln.Cli
Description : Command line interface to nvs.
Copyright   : (c) Piotr Bogdan, 2017
License     : BSD3
Maintainer  : ppbogdan@gmail.com
Stability   : experimental
Portability : Unknown

Command line interface to nvs.

-}

{-# LANGUAGE QuasiQuotes #-}

module Nixpkgs.Vuln.Cli
  ( defaultMain
  ) where

import Protolude

import Control.Monad.Logger
import Data.String (String)
import Nixpkgs.Vuln.Report
import Nixpkgs.Vuln.Cli.Opts
import Options.Applicative (execParser)
import Shell
import System.IO.Temp
import Text.RawString.QQ

defaultMain :: IO ()
defaultMain =
  run =<<
  execParser (parseOptions `withInfo` "Experimental CVE scanner for nixpkgs")

run :: Options -> IO ()
run (Options nvdFeed nixpkgs mode out) =
  runStderrLoggingT $
  withSystemTempDirectory "nvs" $ \tmpDir -> do
    ret <-
      runExceptT $ do
        generateMaintainers nixpkgs tmpDir
        generatePackages nixpkgs tmpDir
    case ret of
      Left e -> panic . show $ e
      Right _ ->
        liftIO $
        report
          (toS nvdFeed)
          (toS tmpDir <> "/packages.json")
          (toS tmpDir <> "/maintainers.json")
          (toS out)
          mode

generateMaintainers ::
     (MonadIO m, MonadLogger m, MonadError ShellCmdFailed m)
  => Text
  -> FilePath
  -> m ()
generateMaintainers nixpkgs tmpDir = do
  shell $
    command "nix-instantiate" $ do
      arg "--eval"
      option
        "-E"
        ("let m = import " <> nixpkgs <>
         "/lib/maintainers.nix; in builtins.toJSON m")
      raw $ " > " <> toS tmpDir <> "/maintainers.json"
  shell $
    command "sed" $ do
      raw . toS $ ([r|-i 's/"{/{/g'|] :: String)
      arg $ toS tmpDir <> "/maintainers.json"
  shell $
    command "sed" $ do
      raw . toS $ ([r|-i 's/}"/}/g'|] :: String)
      arg $ toS tmpDir <> "/maintainers.json"
  shell $
    command "sed" $ do
      raw . toS $ ([r|-i 's/\\"/"/g'|] :: String)
      arg $ toS tmpDir <> "/maintainers.json"
  shell $
    command "sed" $ do
      raw . toS $ ([r|-i 's/\\\\/\\/g'|] :: String)
      arg $ toS tmpDir <> "/maintainers.json"

generatePackages ::
     (MonadIO m, MonadLogger m, MonadError ShellCmdFailed m)
  => Text
  -> FilePath
  -> m ()
generatePackages nixpkgs tmpDir =
  shell $
  command "nix-env" $ do
    switch "--arg"
    raw "config '{}'"
    switch "-qaP"
    switch "--json"
    arg "*"
    option "-f" nixpkgs
    raw $ " > " <> toS tmpDir <> "/packages.json"
