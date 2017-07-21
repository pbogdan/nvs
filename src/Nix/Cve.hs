module Nix.Cve where

import           Protolude

import qualified Data.HashMap.Strict as HashMap
import           Distribution.Package
import           Lucid hiding (for_)
import           Lucid.Base
import           Lucid.Bootstrap
import           Nvd.Cve

report :: FilePath -> FilePath -> IO ()
report cvePath pkgsPath = do
  cves <- parseCves cvePath
  pkgs <- parsePackages pkgsPath
  let byProduct = cvesByProduct cves
      go :: [(Package, Cve)] -> Package -> [(Package, Cve)]
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
      link_ [rel_ "stylesheet", href_ "css/bootstrap.min.css"]
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
          for_ (sortBy (compare `on` packageName . fst) vulns) $ \(pkg, cve) ->
            tr_ $ do
              td_ (toHtml . packageName $ pkg)
              td_ (toHtml . packageVersion $ pkg)
              td_
                (a_
                   [href_ ("https://nvd.nist.gov/vuln/detail/" <> cveId cve)]
                   (toHtml . cveId $ cve))
              td_ (toHtml . cveDescription $ cve)
