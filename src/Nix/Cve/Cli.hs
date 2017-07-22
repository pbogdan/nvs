{-# LANGUAGE QuasiQuotes #-}

module Nix.Cve.Cli where

import Protolude

import Control.Monad.Logger
import Data.String (String)
import Nix.Cve.Report
import Nix.Cve.Cli.Opts
import Options.Applicative (execParser)
import Shell
import System.IO.Temp
import Text.RawString.QQ

defaultMain :: IO ()
defaultMain =
  run =<<
  execParser (parseOptions `withInfo` "Experimental CVE scanner for nixpkgs")

run :: Options -> IO ()
run (Options nvdFeed nixpkgs out) =
  runStderrLoggingT $
  withSystemTempDirectory "nix-cve" $ \tmpDir -> do
    ret <-
      runExceptT $ do
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
        shell $
          command "nix-env" $ do
            switch "--arg"
            raw "config '{}'"
            switch "-qaP"
            switch "--json"
            arg "*"
            option "-f" nixpkgs
            raw $ " > " <> toS tmpDir <> "/packages.json"
    case ret of
      Left e -> panic . show $ e
      Right _ ->
        liftIO $
        report
          (toS nvdFeed)
          (toS tmpDir <> "/packages.json")
          (toS tmpDir <> "/maintainers.json")
          (toS out)
