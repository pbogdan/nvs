module Distribution.Nixpkgs.Packages where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString as Bytes
import qualified Data.HashMap.Strict as HashMap
import           Data.Vector (Vector)
import qualified Data.Vector as Vec
import           Distribution.Package
import           Protolude

parsePackages :: (MonadIO m) => FilePath -> m (Vector Package)
parsePackages path = do
  s <- liftIO . Bytes.readFile $ path
  let parser =
        withObject "Packages" $ \o ->
          sequenceA (parseJSON <$> (Vec.fromList . HashMap.elems $ o))
  let mRet = join (parseEither parser <$> eitherDecodeStrict s)
  return $ either (panic . toS) identity mRet
