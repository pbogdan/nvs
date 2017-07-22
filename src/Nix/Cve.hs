module Nix.Cve where

import           Protolude hiding (link)

import qualified Data.HashMap.Strict as HashMap
import           Data.Set (Set)
import qualified Data.Set as Set
import           Distribution.Package
import           Lucid hiding (for_)
import           Lucid.Base
import           Lucid.Bootstrap
import           Nvd.Cve

-- @TODO: semantics of "-" in package / product version are unclear..
report :: FilePath -> FilePath -> IO ()
report cvePath pkgsPath = do
  cves <- parseCves cvePath
  pkgs <- parsePackages pkgsPath
  let byProduct = cvesByProduct cves
      go :: [(Package, Set Cve)] -> Package -> [(Package, Set Cve)]
      go acc p =
        let pVersion = packageVersion p
            pName = packageName p
        in case ( HashMap.lookup (pName, pVersion) byProduct
                , HashMap.lookup (pName, "*") byProduct) of
             (Nothing, Nothing) -> acc
             (Just cve, Nothing) -> (p, cve) : acc
             (Nothing, Just cve) -> (p, cve) : acc
             (Just cve, Just cve') -> (p, cve) : (p, cve') : acc
      vulns = HashMap.foldl' go [] pkgs
  renderToFile "report.html" $ do
    doctype_
    head_ $ do
      meta_ [makeAttribute "charset" "utf-8"]
      link_
        [ rel_ "stylesheet"
        , href_
            "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
        ]
    body_ $
      container_ $
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
                     [href_ ("https://nvd.nist.gov/vuln/detail/" <> cveId cve)]
                     (toHtml . cveId $ cve))
                td_ (toHtml . cveDescription $ cve)
              tr_ $ do
                td_ [colspan_ "3"] ""
                td_ $ do
                  p_ $ b_ "Package maintainers:"
                  ul_ $
                    for_ (packageMetaMaintainers . packageMeta $ pkg) $ \maintainers ->
                      for_ maintainers $ \maintainer -> li_ (toHtml maintainer)
