{-# LANGUAGE TemplateHaskell #-}

module Nix.Cve.Report.Template
  ( markdownTemplate
  ) where

import Protolude

import Data.FileEmbed

markdownTemplate :: ByteString
markdownTemplate = $(embedFile "templates/cves.ede")
