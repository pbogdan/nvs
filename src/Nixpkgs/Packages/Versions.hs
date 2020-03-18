{-# LANGUAGE TypeApplications #-}

module Nixpkgs.Packages.Versions
  ( versionCandidate
  )
where

import           Protolude

import           Data.List                      ( last
                                                , lookup
                                                , tail
                                                )
import           Data.Versions
import           Nixpkgs.Packages.Types

mean :: (Fractional b, Real a, Foldable t) => t a -> b
mean xs = realToFrac (sum xs) / fromIntegral (length xs)

stddev :: (Floating a, Foldable t, Functor t, Real a) => t a -> a
stddev xs =
  sqrt $ sum (map (\x -> (x - mean xs) ^ (2 :: Int)) xs) / fromIntegral
    (length xs)

commonPrefix :: (Eq e) => [e] -> [e] -> [e]
commonPrefix _  [] = []
commonPrefix [] _  = []
commonPrefix (x : xs) (y : ys) | x == y    = x : commonPrefix xs ys
                               | otherwise = []

permute :: Eq a => [a] -> [[(a, a)]]
permute xs = map (\x -> [ (x, y) | y <- xs, y /= x ]) xs

commonVersionPrefix :: PackageVersion -> PackageVersion -> [VChunk]
commonVersionPrefix (PackageVersion x) (PackageVersion y) =
  let v1 = version x
      v2 = version y
      c1 = _vChunks <$> v1
      c2 = _vChunks <$> v2
  in  case commonPrefix <$> c1 <*> c2 of
        Left  _ -> []
        Right n -> n

withMeanCommonPrefix :: [PackageVersion] -> [(PackageVersion, Double)]
withMeanCommonPrefix vs =
  let xs =
          map (mean @Double . map (length . uncurry commonVersionPrefix))
            . permute
            $ vs
  in  sortBy (compare `on` snd) . zip vs $ xs

taus :: [(Int, Double)]
taus =
  [ (3 , 1.1511)
  , (4 , 1.4250)
  , (5 , 1.5712)
  , (6 , 1.6563)
  , (7 , 1.7110)
  , (8 , 1.7491)
  , (9 , 1.7770)
  , (10, 1.7984)
  , (11, 1.8153)
  , (12, 1.8290)
  , (13, 1.8403)
  , (14, 1.8498)
  , (15, 1.8579)
  , (16, 1.8649)
  , (17, 1.8710)
  , (18, 1.8764)
  , (19, 1.8811)
  , (20, 1.8853)
  , (21, 1.8891)
  , (22, 1.8926)
  , (23, 1.8957)
  , (24, 1.8985)
  , (25, 1.9011)
  , (26, 1.9035)
  , (27, 1.9057)
  , (28, 1.9078)
  , (29, 1.9096)
  , (30, 1.9114)
  , (31, 1.9130)
  , (32, 1.9146)
  , (33, 1.9160)
  , (34, 1.9174)
  , (35, 1.9186)
  , (36, 1.9198)
  , (37, 1.9209)
  , (38, 1.9220)
  , (40, 1.9240)
  , (42, 1.9257)
  , (44, 1.9273)
  , (46, 1.9288)
  , (48, 1.9301)
  , (50, 1.9314)
  ]

tau :: Int -> Maybe Double
tau = flip lookup taus

{-
https://en.wikipedia.org/wiki/Outlier#Modified_Thompson_Tau_test
http://www.mne.psu.edu/cimbala/me345/Lectures/Outliers.pdf
-}
dropOutliers :: [(PackageVersion, Double)] -> [(PackageVersion, Double)]
dropOutliers [] = []
dropOutliers vs@(v : _) =
  let xs             = map snd vs
      m              = mean @Double xs
      s              = stddev xs
      x              = v
      y              = last vs
      absdevx        = abs (snd x - m)
      absdevy        = abs (snd y - m)
      t              = (*) <$> tau (length vs) <*> pure s
      (suspect, dev) = if absdevx > absdevy then (x, absdevx) else (y, absdevy)
      test           = (<=) <$> pure dev <*> t
  in  case test of
        Just False ->
          dropOutliers
            . withMeanCommonPrefix
            . map fst
            . filter ((fst suspect /=) . fst)
            $ vs
        Just True -> vs
        Nothing   -> vs

allEq :: Eq a => [a] -> Bool
allEq xs = and $ zipWith (==) xs (tail xs)

commonPrefixLength :: [PackageVersion] -> Maybe Int
commonPrefixLength vs =
  let xs = map snd . dropOutliers . withMeanCommonPrefix $ vs
  in  if allEq xs
        then case xs of
          []      -> Nothing
          (x : _) -> if x == 0 then Nothing else Just . truncate $ (x + 1)
        else Nothing

versionCandidate :: PackageVersion -> [PackageVersion] -> Maybe PackageVersion
versionCandidate pv vs =
  let cpl = commonPrefixLength vs
  in  case cpl of
        Just l ->
          let xs =
                  filter (\(_, l') -> l' >= l)
                    . map (\v -> (v, length . commonVersionPrefix pv $ v))
                    $ vs
          in  case xs of
                []         -> Nothing
                [(pv', _)] -> Just pv'
                _          -> Nothing
        Nothing -> Nothing
