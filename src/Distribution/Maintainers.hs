{-|
Module      : Distribution.Maintainers
Description : Utilities to parse maintainer information extracted from nixpkgs.
Copyright   : (c) Piotr Bogdan, 2017
License     : BSD3
Maintainer  : ppbogdan@gmail.com
Stability   : experimental
Portability : Unknown

This module provides simple utilities to work with JSON representation of
nixpkgs maintainers that's been extracted from nixpkgs tree.

-}
module Distribution.Maintainers
  ( Maintainer(..)
  , parseMaintainers
  , findMaintainer
  ) where

import           Protolude hiding (handle)

import           Data.Aeson
import qualified Data.ByteString as Bytes
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

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
  { maintainerHandle :: Text
  , maintainerName :: Text
  , maintainerEmail :: Text
  } deriving (Eq, Ord, Show)

-- | Equivalent of fromJust for Either. Partial, therefore unsafe.
fromEither :: Either a b -> b
fromEither (Right x) = x
fromEither (Left _) = panic "fromEither"

-- | Utility function for parsing maintainer information out of a JSON file. The
-- expected structure of the said file is follows:
--
-- > {
-- >   "pbogdan": "Piotr Bogdan <ppbogdan@gmail.com>"
-- > }
--
-- The result is a hash map keyed off maintainers' names.
parseMaintainers ::
     MonadIO m
  => FilePath -- ^ path to the JSON file
  -> m (HashMap Text Maintainer)
parseMaintainers path = do
  s <- liftIO . Bytes.readFile $ path
  let mtsOrErr = eitherDecodeStrict s
  case mtsOrErr of
    Left e -> panic . show $ e
    Right mts ->
      return .
      HashMap.map fromEither .
      HashMap.filter isRight . flip HashMap.mapWithKey mts $ \handle val ->
        case val of
          String mt ->
            Right
              Maintainer
              { maintainerHandle = handle
              , maintainerName = parseMaintainerName mt
              , maintainerEmail = parseMaintainerEmail mt
              }
          _ -> Left ("cannot parse maintainer" :: Text)

-- | Utility function to extract maintainer's name out strings with the
-- following structure:
--
-- > "Piotr Bogdan <ppbogdan@gmail.com>"
parseMaintainerName :: Text -> Text
parseMaintainerName = Text.init . Text.takeWhile (/= '<')

-- | Utility function to extract maintainer's email out strings with the
-- following structure:
--
-- > "Piotr Bogdan <ppbogdan@gmail.com>"
parseMaintainerEmail :: Text -> Text
parseMaintainerEmail = Text.init . Text.takeWhileEnd (/= '<')

-- | Utility function for finding maintainers in results produced by
-- 'parseMaintainers'. The search term is expected to be of the form:
--
-- > "Piotr Bogdan <ppbogdan@gmail.com>"
findMaintainer :: Text -- ^ the search term
  -> HashMap Text Maintainer -- ^ hash map produced by 'parseMaintainers'
  -> Maybe Maintainer
findMaintainer needle mts =
  let name = parseMaintainerName needle
      email = parseMaintainerEmail needle
      matches =
        HashMap.elems . flip HashMap.filter mts $ \mt ->
          maintainerName mt == name && maintainerEmail mt == email
  in case matches of
       [] -> Nothing
       [x] -> Just x
       xs -> head xs
