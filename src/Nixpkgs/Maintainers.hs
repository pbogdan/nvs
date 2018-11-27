{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Nixpkgs.Maintainers
Description : Utilities to parse maintainer information extracted from nixpkgs.
Copyright   : (c) Piotr Bogdan, 2017
License     : BSD3
Maintainer  : ppbogdan@gmail.com
Stability   : experimental
Portability : Unknown

This module provides simple utilities to work with JSON representation of
nixpkgs maintainers that's been extracted from nixpkgs tree.

-}
module Nixpkgs.Maintainers
  ( Maintainer(..)
  ) where

import           Protolude hiding (handle)

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.String (String)

-- | Represents information about nixpkgs maintainer. Given the following entry
-- in lib/maintainers.nix:
--
-- > pbogdan = "Piotr Bogdan <ppbogdan@gmail.com>";
--
-- it will be parsed into:
--
-- > Maintainer
-- > { maintainerHandle = "pbogdan"
-- > , maintainerName = "Piotr Bogdan"
-- > , maintainerEmail = "ppbogdan@gmail.com"
-- > }
data Maintainer = Maintainer
  { maintainerGithub :: Maybe Text
  , maintainerName :: Text
  , maintainerEmail :: Text
  } deriving (Eq, Generic, Ord, Show)

instance FromJSON Maintainer where
  parseJSON =
    genericParseJSON $ aesonDrop (length ("Maintainer" :: String)) camelCase

instance ToJSON Maintainer where
  toJSON = genericToJSON $ aesonDrop (length ("Maintainer" :: String)) camelCase

