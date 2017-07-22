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

data Maintainer = Maintainer
  { maintainerHandle :: Text
  , maintainerName :: Text
  , maintainerEmail :: Text
  } deriving (Eq, Ord, Show)

fromEither :: Either a b -> b
fromEither (Right x) = x
fromEither (Left _) = panic "fromEither"

parseMaintainers :: MonadIO m => FilePath -> m (HashMap Text Maintainer)
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

parseMaintainerName :: Text -> Text
parseMaintainerName = Text.init . Text.takeWhile (/= '<')

parseMaintainerEmail :: Text -> Text
parseMaintainerEmail = Text.init . Text.takeWhileEnd (/= '<')

findMaintainer :: Text -> HashMap Text Maintainer -> Maybe Maintainer
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
