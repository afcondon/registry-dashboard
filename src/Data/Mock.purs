-- | Data.Mock
-- |
-- | Mock data for the Registry Dashboard prototype.
-- | Simulates realistic compiler compatibility patterns.
module Data.Mock
  ( mockMatrix
  , mockPackages
  , mockCompilers
  , mockJobs
  ) where

import Prelude

import Data.Array (concat, mapWithIndex, index)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Types (CompatibilityMatrix, MatrixJob, CompatibilityStatus(..))

-- | Sample packages representing common registry packages
mockPackages :: Array String
mockPackages =
  [ "prelude"
  , "effect"
  , "console"
  , "arrays"
  , "maybe"
  , "either"
  , "tuples"
  , "strings"
  , "foldable-traversable"
  , "transformers"
  , "aff"
  , "halogen"
  , "argonaut"
  , "foreign"
  , "node-fs"
  , "node-buffer"
  , "http"
  , "quickcheck"
  , "spec"
  , "parsing"
  , "routing"
  , "codec"
  , "datetime"
  , "ordered-collections"
  , "profunctor"
  , "variant"
  , "record"
  , "affjax"
  , "yoga-json"
  , "web-dom"
  ]

-- | Compiler versions (newest first)
mockCompilers :: Array String
mockCompilers =
  [ "0.15.15"
  , "0.15.14"
  , "0.15.10"
  , "0.15.9"
  , "0.15.7"
  , "0.15.4"
  ]

-- | Generate the mock compatibility matrix
-- | Simulates realistic patterns:
-- | - Core packages (prelude, effect, etc.) compatible everywhere
-- | - Newer packages may have gaps with older compilers
-- | - Some random failures and untested cells
mockMatrix :: CompatibilityMatrix
mockMatrix =
  { packages: mockPackages
  , compilers: mockCompilers
  , values: mockValues
  }

-- | Mock matrix values
-- | 1.0 = compatible, 0.0 = failed, 0.5 = untested
mockValues :: Array (Array Number)
mockValues =
  -- Each row is a package, each column is a compiler version
  -- Columns: 0.15.15, 0.15.14, 0.15.10, 0.15.9, 0.15.7, 0.15.4
  [ [1.0, 1.0, 1.0, 1.0, 1.0, 1.0]  -- prelude: always works
  , [1.0, 1.0, 1.0, 1.0, 1.0, 1.0]  -- effect: always works
  , [1.0, 1.0, 1.0, 1.0, 1.0, 1.0]  -- console: always works
  , [1.0, 1.0, 1.0, 1.0, 1.0, 1.0]  -- arrays: always works
  , [1.0, 1.0, 1.0, 1.0, 1.0, 1.0]  -- maybe: always works
  , [1.0, 1.0, 1.0, 1.0, 1.0, 1.0]  -- either: always works
  , [1.0, 1.0, 1.0, 1.0, 1.0, 1.0]  -- tuples: always works
  , [1.0, 1.0, 1.0, 1.0, 1.0, 1.0]  -- strings: always works
  , [1.0, 1.0, 1.0, 1.0, 1.0, 1.0]  -- foldable-traversable: always works
  , [1.0, 1.0, 1.0, 1.0, 1.0, 1.0]  -- transformers: always works
  , [1.0, 1.0, 1.0, 1.0, 1.0, 1.0]  -- aff: always works
  , [1.0, 1.0, 1.0, 1.0, 0.0, 0.0]  -- halogen: newer only
  , [1.0, 1.0, 1.0, 1.0, 1.0, 0.5]  -- argonaut: mostly works
  , [1.0, 1.0, 1.0, 1.0, 1.0, 1.0]  -- foreign: always works
  , [1.0, 1.0, 1.0, 1.0, 1.0, 0.5]  -- node-fs: mostly works
  , [1.0, 1.0, 1.0, 1.0, 1.0, 0.5]  -- node-buffer: mostly works
  , [1.0, 1.0, 1.0, 0.0, 0.0, 0.0]  -- http: recent only
  , [1.0, 1.0, 1.0, 1.0, 1.0, 1.0]  -- quickcheck: always works
  , [1.0, 1.0, 1.0, 1.0, 0.5, 0.5]  -- spec: mostly recent
  , [1.0, 1.0, 1.0, 1.0, 1.0, 0.5]  -- parsing: mostly works
  , [1.0, 1.0, 0.0, 0.0, 0.0, 0.0]  -- routing: very new
  , [1.0, 1.0, 1.0, 1.0, 0.5, 0.5]  -- codec: recent focus
  , [1.0, 1.0, 1.0, 1.0, 1.0, 1.0]  -- datetime: always works
  , [1.0, 1.0, 1.0, 1.0, 1.0, 1.0]  -- ordered-collections: always works
  , [1.0, 1.0, 1.0, 1.0, 1.0, 1.0]  -- profunctor: always works
  , [1.0, 1.0, 1.0, 1.0, 0.5, 0.0]  -- variant: recent
  , [1.0, 1.0, 1.0, 1.0, 1.0, 0.5]  -- record: mostly works
  , [1.0, 1.0, 0.0, 0.0, 0.0, 0.0]  -- affjax: recent rewrite
  , [1.0, 1.0, 1.0, 0.5, 0.0, 0.0]  -- yoga-json: newer package
  , [1.0, 1.0, 1.0, 1.0, 1.0, 1.0]  -- web-dom: always works
  ]

-- | Mock matrix jobs simulating recent build activity
-- | Jobs are spread over the past 24 hours
mockJobs :: Array MatrixJob
mockJobs =
  let
    baseTime = 1738500000000.0  -- ~Feb 2, 2025
    hourMs = 3600000.0

    -- Generate jobs for each package/compiler combination that passed or failed
    generateJobs :: Int -> String -> Array MatrixJob
    generateJobs pkgIdx pkg =
      mapWithIndex (mkJob pkgIdx pkg) mockCompilers

    mkJob :: Int -> String -> Int -> String -> MatrixJob
    mkJob pkgIdx pkg compIdx compiler =
      let
        -- Pseudo-random time offset based on indices
        hourOffset = toNumber ((pkgIdx * 7 + compIdx * 13) `mod` 24)
        minuteOffset = toNumber ((pkgIdx * 11 + compIdx * 17) `mod` 60)
        timestamp = baseTime - (hourOffset * hourMs) - (minuteOffset * 60000.0)

        -- Duration varies by "complexity" (longer for later packages)
        duration = 30.0 + toNumber (pkgIdx `mod` 5) * 20.0 + toNumber compIdx * 10.0

        -- Status from matrix
        statusVal = case index mockValues pkgIdx of
          Just row -> case index row compIdx of
            Just v -> v
            Nothing -> 0.5
          Nothing -> 0.5
        status = if statusVal >= 0.75 then Compatible
                 else if statusVal <= 0.25 then Failed
                 else Untested
      in
        { id: pkgIdx * 10 + compIdx
        , package: pkg
        , compiler
        , status
        , timestamp
        , duration
        }
  in
    concat $ mapWithIndex generateJobs mockPackages
