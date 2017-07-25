{-|
Module      : Nixpkgs.Vuln.Report
Description : Report rendering utilities.
Copyright   : (c) Piotr Bogdan, 2017
License     : BSD3
Maintainer  : ppbogdan@gmail.com
Stability   : experimental
Portability : Unknown

Report rendering utilities.

-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Nixpkgs.Vuln.Report
  ( RenderMode(..)
  , report
  ) where

import           Protolude hiding (link)

import           Data.Aeson
import           Data.Aeson.Casing
import qualified Data.ByteString as Bytes
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String (String)
import           Lucid hiding (for_)
import           Lucid.Base
import           Lucid.Bootstrap
import           Nixpkgs.Vuln.Report.Template
import           Nixpkgs.Maintainers
import           Nixpkgs.Packages
import           Nvd.Cve
import           Text.EDE

-- | Specifies rendering mode, or more precisely the output format.
data RenderMode
  = HTML
  | Markdown
  deriving (Eq, Show)

data CveWithPackage = CveWithPackage
  { _cveWithPackageCve :: Cve
  , _cveWithPackagePackage :: Package
  , _cveWithPackageMaintainers :: [Maintainer]
  } deriving (Eq, Generic, Show)

instance ToJSON CveWithPackage where
  toJSON =
    genericToJSON $ aesonDrop (length ("_CveWithPackage" :: String)) camelCase

-- @TODO: semantics of "-" in package / product version are unclear..

-- | Produce a human readable report about CVEs that may be present in the given
-- package set.
--
-- To see how the packages.json and mainers.json files are generated please
-- refer to "Nixpkgs.Vuln.Cli" module.
report ::
     FilePath -- ^ path to NVD JSON feed
  -> FilePath -- ^ path to packages.json file
  -> FilePath -- ^ path to maintainers.json file
  -> FilePath -- ^ output path for the generated report
  -> RenderMode -- ^ what type of output to generate
  -> IO ()
report cvePath pkgsPath mtsPath outPath mode = do
  cves <- parseCves cvePath
  pkgs <- parsePackages pkgsPath
  mts <- parseMaintainers mtsPath
  let byProduct = cvesByProduct cves
      go :: [(Package, Set Cve)] -> Package -> [(Package, Set Cve)]
      go acc p =
        let pVersion = packageVersion p
            pName = packageName p
            matches =
              map (p, ) $
              catMaybes
                [ HashMap.lookup (pName, pVersion) byProduct
                , HashMap.lookup (pName, "*") byProduct
                ]
        in acc ++ matches
      vulns = HashMap.foldl' go [] pkgs
      renderer =
        case mode of
          HTML -> renderHTML
          Markdown -> renderMarkdown
  renderer vulns mts outPath

renderHTML ::
     [(Package, Set Cve)] -> HashMap Text Maintainer -> FilePath -> IO ()
renderHTML vulns mts outPath =
  renderToFile outPath $ do
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
        h1_ "Introduction"
        p_
          "This is POC that performs a scan against JSON feed from National Vulnerability Database https://nvd.nist.gov/vuln/data-feeds#JSON_FEED and recent-ish checkout of release-17.03 branch of nixpkgs."
        p_
          "As this is an early prototype it may not be completely accurate. It also doesn't scan all of packages such as those nested deeper in the attribute hierarchy of nixpkgs. Not all of CVEs may be relevant as well so addressing them would require further manual review - for example if a security patch has been applied without corresponding version bump of the package."
        p_
          "When listing maintainers the assumption that their handle is a valid GitHub username may not hold."
        h2_ "Potential CVEs"
        table_ [class_ "table"] $ do
          thead_ $
            tr_ $ do
              th_ [width_ "15%"] "Package name"
              th_ [width_ "15%"] "Package version"
              th_ [width_ "15%"] "CVE ID"
              th_ "CVE description"
          tbody_ $
            for_ (sortBy (compare `on` packageName . fst) vulns) $ \(pkg, cves') ->
              for_ (Set.toAscList cves') $ \cve -> do
                tr_ $ do
                  td_ $ do
                    let pName = packageName pkg
                        mLink = packageUrl pkg
                        html =
                          case mLink of
                            Nothing -> toHtml pName
                            Just link -> a_ [href_ link] (toHtml pName)
                    html
                  td_ (toHtml . packageVersion $ pkg)
                  td_
                    (a_
                       [ href_
                           ("https://nvd.nist.gov/vuln/detail/" <> cveId cve)
                       ]
                       (toHtml . cveId $ cve))
                  td_ (toHtml . cveDescription $ cve)
                tr_ $ do
                  td_ [colspan_ "3"] ""
                  td_ $ do
                    p_ $ b_ "Package maintainers:"
                    ul_ $
                      for_ (packageMetaMaintainers . packageMeta $ pkg) $ \maintainers ->
                        for_ maintainers $ \maintainer ->
                          li_ (renderMaintainer maintainer mts)

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
     [(Package, Set Cve)] -> HashMap Text Maintainer -> FilePath -> IO ()
renderMarkdown vulns mts outPath = do
  let cves' =
        map (uncurry3 CveWithPackage) .
        sortBy (compare `on` packageName . snd3) .
        concatMap
          ((\(p, cves) ->
              map (\cve -> (cve, p, findMaintersForPackage p mts)) cves) .
           second Set.toAscList) $
        vulns
      Just env =
        fromValue . toJSON . HashMap.fromList $ [("cves" :: Text, cves')]
  let tpl = eitherParse markdownTemplate
  let ret = flip eitherRender env =<< tpl
  case ret of
    Left e -> do
      putText $ "Rendering failed: " <> toS e
      exitFailure
    Right out -> Bytes.writeFile (toS outPath) (toS out)
  where
    snd3 :: (a, b, c) -> b
    snd3 (_, b, _) = b
    uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
    uncurry3 f ~(a, b, c) = f a b c
