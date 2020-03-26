{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nvs.Report
  ( Output(..)
  , report
  )
where

import           Protolude               hiding ( link
                                                , packageName
                                                )


import           Control.Concurrent.Async       ( forConcurrently_
                                                , forConcurrently
                                                )
import           Control.Concurrent.STM
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource   ( runResourceT )
import           Nvs.Render                     ( renderMarkdown
                                                , renderHTML
                                                , renderJSON
                                                )
import           Data.Attoparsec.Text           ( IResult(..) )
import qualified Data.Attoparsec.Text          as Parsec
                                                ( parse )
import qualified Data.ByteString               as Bytes
import qualified Data.ByteString.Streaming     as Stream
                                         hiding ( filter
                                                , map
                                                )
import qualified Data.ByteString.Streaming.Aeson
                                               as Stream
import           Data.Char                      ( isDigit )
import           Data.Foldable                  ( foldr1 )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import           Data.JsonStream.Parser  hiding ( string )
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Data.Text.IO                  as Text
import           Filesystem.Path.CurrentOS      ( encodeString )
import qualified Nix.Derivation                as Derivation
import           Nixpkgs.Packages
import           Nixpkgs.Packages.Types
import           Nvd.Cpe
import           Nvd.Cpe.Configuration
import           Nvd.Cve
import qualified Streaming.Prelude             as Stream
                                         hiding ( readFile )
import           Text.Regex.Applicative

data Output
  = HTML
  | JSON
  | Markdown
  deriving (Eq, Show)

report
  :: (MonadLogger m, MonadIO m)
  => [FilePath] -- ^ paths to NVD JSON feeds
  -> FilePath -- ^ path to the store derivation to scan
  -> Output -- ^ what type of output to generate
  -> m ()
report cvePaths drvPath mode = do
  logInfoN "Collecting derivation inputs..."
  pkgs <- liftIO . collectDerivationInputs $ drvPath
  logInfoN "Done!"
  let
    (excludes :: [CveId]) = foldr
      (\cves acc -> (concatMap packagePatches . Set.toList $ cves) ++ acc)
      []
      pkgs
    parser = "CVE_Items" .: arrayOf value :: Parser (Cve (Configuration Cpe))
    go path =
      Stream.readFile path
        & Stream.streamParse parser
        & void
        & Stream.filter (\cve -> cveId cve `notElem` excludes)
        & Stream.map
            (\cve -> zip (Set.toList . matchMany pkgs $ cve) (repeat cve))
        & Stream.mconcat_
    renderer = case mode of
      HTML     -> renderHTML
      Markdown -> renderMarkdown
      JSON     -> renderJSON
  logInfoN "Processing the feeds..."
  matches <- mconcat <$> liftIO (forConcurrently cvePaths (runResourceT . go))
  logInfoN "Done!"
  renderer matches

nofailParseDerivation :: Text -> Derivation.Derivation
nofailParseDerivation t =
  let (Done _ !drv) = Parsec.parse Derivation.parseDerivation t in drv

{-

@TODO: create a separate executable to profile this function as parsing derivation is taking way too
long than what I would expect on paper; indeed it is slower than than streaming gobs of JSON and all
the matching etc.

-}
collectDerivationInputs
  :: FilePath -> IO (HashMap PackageName (Set (Package CveId)))
collectDerivationInputs path = do
  pkgs <-
    liftIO
    . newTVarIO
    $ (HashMap.empty :: HashMap PackageName (Set (Package CveId)))
  seen <- liftIO . newTVarIO $ Set.empty
  go pkgs seen path
  readTVarIO pkgs
 where
  go pkgs seen file = do
    t <- Text.readFile file
    let drv        = nofailParseDerivation t
        inputs     = Map.keys . Derivation.inputDrvs $ drv
        drvName    = fromMaybe "" . Map.lookup "name" . Derivation.env $ drv
        isFOD      = Map.member "outputHash" . Derivation.env $ drv
        pkgName    = parsePackageName drvName
        pkgVersion = parsePackageVersion drvName
        drvPatches = fromMaybe "" . Map.lookup "patches" . Derivation.env $ drv
        patchNames = Bytes.split (fromIntegral . ord $ ' ') . toS $ drvPatches
        cvePatches = mapMaybe
          ( (=~ many anySym
              *> (CveId . toS <$> foldr1
                   (liftA2 (<>))
                   [ string "CVE-"
                   , some (psym isDigit)
                   , string "-"
                   , some (psym isDigit)
                   ]
                 )
              <* many anySym
            )
          . toS
          )
          patchNames
    unless (pkgVersion == "" || isFOD) $ atomically $ modifyTVar'
      pkgs
      (HashMap.insertWith
        Set.union
        pkgName
        (Set.singleton
          (Package { packageName    = pkgName
                   , packageVersion = pkgVersion
                   , packagePatches = cvePatches
                   }
          )
        )
      )
    forConcurrently_ inputs $ \input -> do
      inputSeen <- Set.member input <$> readTVarIO seen
      unless inputSeen $ do
        atomically $ modifyTVar' seen (Set.insert input)
        go pkgs seen . encodeString $ input
