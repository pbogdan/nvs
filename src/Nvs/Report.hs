{-|
Module      : Nvs.Report
Description : Report rendering utilities.
Copyright   : (c) Piotr Bogdan, 2017
License     : BSD3
Maintainer  : ppbogdan@gmail.com
Stability   : experimental
Portability : Unknown

Report rendering utilities.

-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Nvs.Report
  ( Output(..)
  , Matching(..)
  , report
  ) where

import           Protolude hiding (link)

import           Control.Monad.Logger
import           Data.Aeson hiding ((.:))
import           Data.Aeson.Casing
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Streaming as Stream (readFile)
import qualified Data.ByteString.Streaming.Aeson as Stream
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.JsonStream.Parser
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String (String)
import           Lucid hiding (for_, term)
import           Lucid.Base hiding (term)
import           Lucid.Bootstrap
import           Nixpkgs.Maintainers
import           Nixpkgs.Packages
import           Nixpkgs.Packages.Aliases
import           Nixpkgs.Packages.Types
import           Nvd.Cpe.Configuration
import           Nvd.Cve
import           Nvs.Excludes
import           Nvs.Files
import           Nvs.Types
import qualified Streaming as Stream
import qualified Streaming.Prelude as Stream hiding (readFile)
import           Text.EDE


-- | Specifies rendering mode, or more precisely the output format.
data Output
  = HTML
  | Markdown
  deriving (Eq, Show)

data Matching
  = Simple
  | Cpe
  deriving (Eq, Show)

data CveWithPackage a = CveWithPackage
  { _cveWithPackageCve :: Cve a
  , _cveWithPackagePackage :: Package
  , _cveWithPackageMaintainers :: [Maintainer]
  } deriving (Eq, Generic, Show)

instance ToJSON (Cve a) => ToJSON (CveWithPackage a) where
  toJSON =
    genericToJSON $ aesonDrop (length ("_CveWithPackage" :: String)) camelCase

dropNvdExcludes ::
     Excludes -> HashMap k (Set (Cve a)) -> HashMap k (Set (Cve a))
dropNvdExcludes es =
  let toDrop = Set.fromList . nvdExcludes $ es
  in HashMap.filter (not . Set.null) .
     HashMap.map (Set.filter ((`Set.notMember` toDrop) . cveId))

-- | Produce a human readable report about CVEs that may be present in the given
-- package set.
--
-- To see how the packages.json and mainers.json files are generated please
-- refer to "Nvs.Cli" module.
report ::
     (MonadError NvsError m, MonadLogger m, MonadIO m)
  => [FilePath] -- ^ path to NVD JSON feed
  -> FilePath -- ^ path to packages.json file
  -> FilePath -- ^ path to maintainers.json file
  -> FilePath -- ^ output path for the generated report
  -> Output -- ^ what type of output to generate
  -> Matching
  -> m ()
report cvePaths pkgsPath mtsPath outPath mode matchMode = do
  logInfoN "Parsing excludes"
  es <- parseExcludes =<< findFile "data/vuln-excludes.yaml"
  logInfoN "Parsing NVD feed"
  logInfoN "Parsing packages"
  pkgs <- parsePackages pkgsPath
  logInfoN "Parsing maintainers"
  mts <- parseMaintainers mtsPath
  logInfoN "Parsing aliases"
  aliases <- parseAliases =<< findFile "data/package-aliases.yaml"
  case matchMode of
    Simple -> do
      vulns <-
        mconcat <$>
        for
          cvePaths
          (\path ->
             (dropNvdExcludes es <$> parseCves path) >>= \cves ->
               return (vulnsFor @VendorData pkgs aliases cves))
      case mode of
        HTML -> renderHTML vulns mts outPath
        Markdown -> renderMarkdown vulns mts outPath
    Cpe -> do
      let parser = "CVE_Items" .: arrayOf value :: Parser (Cve CpeConfiguration)
          go path =
            Stream.readFile path & Stream.streamParse parser & void &
            Stream.filter (\cve -> cveId cve `notElem` nvdExcludes es) &
            Stream.map (\cve -> vulnsFor' cve pkgs aliases) &
            Stream.filter (not . null . filter (not . Set.null . snd)) &
            Stream.mconcat_
      vulns <-
        mconcat <$>
        liftIO (forConcurrently cvePaths (Stream.runResourceT . go))
      case mode of
        HTML -> renderHTML vulns mts outPath
        Markdown -> renderMarkdown vulns mts outPath

renderHTML ::
     MonadIO m
  => [(Package, Set (Cve a))]
  -> HashMap Text Maintainer
  -> FilePath
  -> m ()
renderHTML vulns mts outPath =
  liftIO . renderToFile outPath $ do
    doctype_
    head_ $ do
      meta_ [makeAttribute "charset" "utf-8"]
      link_
        [ rel_ "stylesheet"
        , href_
            "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
        ]
    body_ $
      container_ $ do
        h1_ "Potential CVEs"
        table_ [class_ "table"] $ do
          thead_ $
            tr_ $ do
              th_ [width_ "15%"] "Package name"
              th_ [width_ "15%"] "Package version"
              th_ [width_ "15%"] "CVE ID"
              th_ "CVE description"
              th_ "Severity"
          tbody_ $
            for_ (sortBy (compare `on` packageName . fst) vulns) $ \(pkg, cves') ->
              for_ (Set.toAscList cves') $ \cve -> do
                tr_ $ do
                  td_ $ do
                    let pName = packageName pkg
                        mLink = packageUrl pkg
                        html =
                          case mLink of
                            Nothing -> toHtml . displayPackageName $ pName
                            Just link ->
                              a_
                                [href_ link]
                                (toHtml . displayPackageName $ pName)
                    html
                  td_ (toHtml . displayPackageVersion . packageVersion $ pkg)
                  td_
                    (a_
                       [ href_
                           ("https://nvd.nist.gov/vuln/detail/" <>
                            (displayCveId . cveId $ cve))
                       , target_ "blank"
                       ]
                       (toHtml . displayCveId . cveId $ cve))
                  td_ (toHtml . cveDescription $ cve)
                  td_ (renderSeverity . cveSeverity $ cve)
                tr_ $ do
                  td_ [colspan_ "3"] ""
                  td_ $ do
                    p_ $ b_ "Package maintainers:"
                    ul_ $
                      for_ (packageMetaMaintainers . packageMeta $ pkg) $ \maintainers ->
                        for_ maintainers $ \maintainer ->
                          li_ (renderMaintainer maintainer mts)
                  td_ [] ""

renderSeverity :: Monad m => Maybe Severity -> HtmlT m ()
renderSeverity severity =
  let label =
        case severity of
          Nothing -> "label-default"
          Just Low -> "label-info"
          Just Medium -> "label-warning"
          Just High -> "label-danger"
          Just Critical -> "label-danger"
      (text :: Text) =
        case severity of
          Nothing -> "unknown"
          Just Low -> "Low"
          Just Medium -> "Medium"
          Just High -> "High"
          Just Critical -> "Critical"
  in span_ [classes_ ["label", label]] (toHtml text)

renderMaintainer :: Monad m => Text -> HashMap Text Maintainer -> HtmlT m ()
renderMaintainer mt mts =
  let mMt = findMaintainer mt mts
  in case mMt of
       Nothing -> toHtml mt
       Just mt' ->
         toHtml (maintainerName mt') <> toHtml (" " :: Text) <>
         toHtml ("<" :: Text) <>
         a_
           [href_ ("mailto:" <> maintainerEmail mt')]
           (toHtml (maintainerEmail mt')) <>
         toHtml (">" :: Text) <>
         toHtml (" " :: Text) <>
         a_
           [href_ ("https://github.com/" <> maintainerHandle mt')]
           (toHtml ("@" <> maintainerHandle mt'))

renderMarkdown ::
     (ToJSON a, MonadIO m)
  => [(Package, Set (Cve a))]
  -> HashMap Text Maintainer
  -> FilePath
  -> m ()
renderMarkdown vulns mts outPath = do
  let cves' =
        map (uncurry3 CveWithPackage) .
        sortBy (compare `on` cveId . fst3) .
        concatMap
          ((\(p, cves) ->
              map (\cve -> (cve, p, findMaintersForPackage p mts)) cves) .
           second Set.toAscList) $
        vulns
      Just env =
        fromValue . toJSON . HashMap.fromList $ [("cves" :: Text, cves')]
  tpl <- liftIO . eitherParseFile =<< findFile "templates/cves.ede"
  let ret = flip eitherRender env =<< tpl
  case ret of
    Left e -> do
      putText $ "Rendering failed: " <> toS e
      liftIO exitFailure
    Right out -> liftIO $ Bytes.writeFile (toS outPath) (toS out)
  where
    fst3 :: (a, b, c) -> a
    fst3 (a, _, _) = a
    uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
    uncurry3 f ~(a, b, c) = f a b c
