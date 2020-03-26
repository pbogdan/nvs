{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Nvs.Report
  ( Output(..)
  , report
  )
where

import           Protolude               hiding ( link
                                                , packageName
                                                )


import           Control.Concurrent.Async       ( forConcurrently )
import           Control.Concurrent.STM
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource   ( runResourceT )
import           Data.Aeson              hiding ( (.:) )
import           Data.Aeson.Casing
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
import           Data.String                    ( String )
import qualified Data.Text.IO                  as Text
import qualified Filesystem.Path               as Filesystem
                                                ( FilePath )
import           Filesystem.Path.CurrentOS      ( encodeString )
import           Lucid                   hiding ( for_
                                                , term
                                                )
import           Lucid.Base              hiding ( term )
import           Lucid.Bootstrap
import qualified Nix.Derivation                as Derivation
import           Nixpkgs.Packages
import           Nixpkgs.Packages.Types
import           Nvd.Cpe
import           Nvd.Cpe.Configuration
import           Nvd.Cve
import           Nvs.Files
import qualified Streaming.Prelude             as Stream
                                         hiding ( readFile )
import           Text.EDE
import           Text.Regex.Applicative

data Output
  = HTML
  | JSON
  | Markdown
  deriving (Eq, Show)

data CveWithPackage a b = CveWithPackage
  { _cveWithPackagePackage :: Package b
  , _cveWithPackageCve :: Cve a
  } deriving (Eq, Generic, Show)

instance (ToJSON a, ToJSON b) => ToJSON (CveWithPackage a b) where
  toJSON =
    genericToJSON $ aesonDrop (length ("_CveWithPackage" :: String)) camelCase

report
  :: (MonadLogger m, MonadIO m)
  => [FilePath] -- ^ paths to NVD JSON feeds
  -> FilePath -- ^ path to the store derivation to scan
  -> Output -- ^ what type of output to generate
  -> m ()
report cvePaths drvPath mode = do
  seen <- liftIO . newTVarIO $ Set.empty
  pkgs <-
    liftIO
    . newTVarIO
    $ (HashMap.empty :: HashMap PackageName (Set (Package CveId)))
  logInfoN "Collecting derivation inputs..."
  liftIO . collectDerivationInputs pkgs seen $ drvPath
  logInfoN "Done!"
  foos <- liftIO . readTVarIO $ pkgs
  let (ex :: [CveId]) = foldr
        (\cves acc -> (concatMap packagePatches . Set.toList $ cves) ++ acc)
        []
        foos
  -- print ex
  let
    parser = "CVE_Items" .: arrayOf value :: Parser (Cve (Configuration Cpe))
    go path =
      Stream.readFile path
        & Stream.streamParse parser
        & void
        & Stream.filter (\cve -> cveId cve `notElem` ex)
        & Stream.map
            (\cve -> zip (Set.toList . matchMany foos $ cve) (repeat cve))
        & Stream.mconcat_
  logInfoN "Processing the feeds..."
  vulns <- mconcat <$> liftIO (forConcurrently cvePaths (runResourceT . go))
  logInfoN "Done!"
  case mode of
    HTML     -> renderHTML vulns
    Markdown -> renderMarkdown vulns
    JSON     -> renderJSON vulns

nofailParseDerivation :: Text -> Derivation.Derivation
nofailParseDerivation t =
  let (Done _ !drv) = Parsec.parse Derivation.parseDerivation t in drv

{-

@TODO: fold the tvars into this function as they are an internal detail really

@TODO: create a separate executable to profile this function as parsing derivation is taking way too
long than what I would expect on paper; indeed it is slower than than streaming gobs of JSON and all
the matching etc.

-}
collectDerivationInputs
  :: TVar (HashMap PackageName (Set (Package CveId)))
  -> TVar (Set Filesystem.FilePath)
  -> FilePath
  -> IO ()
collectDerivationInputs pkgs seen path = do
  t <- Text.readFile path
  let drv        = nofailParseDerivation t
      inputs     = Set.fromList . Map.keys . Derivation.inputDrvs $ drv
      drvName    = fromMaybe "" . Map.lookup "name" . Derivation.env $ drv
      isFOD      = Map.member "outputHash" . Derivation.env $ drv
      pkgName    = parsePackageName drvName
      pkgVersion = parsePackageVersion drvName
      drvPatches = fromMaybe "" . Map.lookup "patches" . Derivation.env $ drv
      patches    = Bytes.split (fromIntegral . ord $ ' ') . toS $ drvPatches
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
        patches
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
  for_ inputs $ \input -> do
    (inputSeen :: Bool) <- Set.member input <$> readTVarIO seen
    unless inputSeen $ do
      atomically $ modifyTVar' seen (Set.insert input)
      liftIO . collectDerivationInputs pkgs seen . toS . encodeString $ input

-- @TODO: restore sorting / ordering for HTML & JSON output
renderHTML :: MonadIO m => [(Package a, Cve b)] -> m ()
renderHTML vulns = putText . toS . renderText $ do
  doctype_
  head_ $ do
    meta_ [makeAttribute "charset" "utf-8"]
    link_
      [ rel_ "stylesheet"
      , href_
        "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
      ]
  body_ $ container_ $ do
    h1_ "Potential CVEs"
    table_ [class_ "table"] $ do
      thead_ $ tr_ $ do
        th_ [width_ "15%"] "Package name"
        th_ [width_ "15%"] "Package version"
        th_ [width_ "15%"] "CVE ID"
        th_ "CVE description"
        th_ "Severity"
      tbody_
        $ for_ (sortBy (compare `on` cvePublished . snd) vulns)
        $ \(pkg, cve) -> tr_ $ do
            td_ $ do
              let pName = packageName pkg
                  html  = toHtml . displayPackageName $ pName
              html
            td_ (toHtml . displayPackageVersion . packageVersion $ pkg)
            td_
              (a_
                [ href_
                  (  "https://nvd.nist.gov/vuln/detail/"
                  <> (displayCveId . cveId $ cve)
                  )
                , target_ "blank"
                ]
                (toHtml . displayCveId . cveId $ cve)
              )
            td_ (toHtml . cveDescription $ cve)
            td_ (renderSeverity . cveSeverity $ cve)

renderSeverity :: Monad m => Maybe Severity -> HtmlT m ()
renderSeverity severity =
  let label = case severity of
        Nothing       -> "label-default"
        Just Low      -> "label-info"
        Just Medium   -> "label-warning"
        Just High     -> "label-danger"
        Just Critical -> "label-danger"
      (text :: Text) = case severity of
        Nothing       -> "unknown"
        Just Low      -> "Low"
        Just Medium   -> "Medium"
        Just High     -> "High"
        Just Critical -> "Critical"
  in  span_ [classes_ ["label", label]] (toHtml text)

renderMarkdown
  :: (ToJSON a, ToJSON b, MonadIO m) => [(Package a, Cve b)] -> m ()
renderMarkdown cves = do
  let Just env =
        fromValue
          . toJSON
          . HashMap.fromList
          $ [("cves" :: Text, map (uncurry CveWithPackage) cves)]
  tpl <- liftIO . eitherParseFile =<< findFile "templates/cves.ede"
  let ret = flip eitherRender env =<< tpl
  case ret of
    Left e -> do
      putText $ "Rendering failed: " <> toS e
      liftIO exitFailure
    Right out -> putText . toS $ out

renderJSON :: (ToJSON a, ToJSON b, MonadIO m) => [(Package a, Cve b)] -> m ()
renderJSON = putText . toS . encode . map (uncurry CveWithPackage)
