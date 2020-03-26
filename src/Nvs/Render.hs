{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nvs.Render
  ( renderMarkdown
  , renderHTML
  , renderJSON
  )
where

import           Protolude               hiding ( link
                                                , packageName
                                                )


import           Data.Aeson              hiding ( (.:) )
import           Data.Aeson.Casing
import qualified Data.HashMap.Strict           as HashMap
import           Data.String                    ( String )
import           Lucid                   hiding ( for_
                                                , term
                                                )
import           Lucid.Base              hiding ( term )
import           Lucid.Bootstrap
import           Nixpkgs.Packages
import           Nixpkgs.Packages.Types
import           Nvd.Cve
import           Nvs.Files
import           Text.EDE

data CveMatch a b = CveMatch
  { _cveMatchPackage :: Package b
  , _cveMatchCve :: Cve a
  } deriving (Eq, Generic, Show)

instance (ToJSON a, ToJSON b) => ToJSON (CveMatch a b) where
  toJSON = genericToJSON $ aesonDrop (length ("_CveMatch" :: String)) camelCase

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
          $ [("cves" :: Text, map (uncurry CveMatch) cves)]
  tpl <- liftIO . eitherParseFile =<< findFile "templates/cves.ede"
  let ret = flip eitherRender env =<< tpl
  case ret of
    Left e -> do
      putText $ "Rendering failed: " <> toS e
      liftIO exitFailure
    Right out -> putText . toS $ out

renderJSON :: (ToJSON a, ToJSON b, MonadIO m) => [(Package a, Cve b)] -> m ()
renderJSON = putText . toS . encode . map (uncurry CveMatch)
