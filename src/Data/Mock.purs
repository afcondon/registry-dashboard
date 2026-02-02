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

-- | Compiler versions (newest first), spanning multiple major versions
mockCompilers :: Array String
mockCompilers =
  -- 0.15.x (6 versions)
  [ "0.15.15"
  , "0.15.14"
  , "0.15.10"
  , "0.15.9"
  , "0.15.7"
  , "0.15.4"
  -- 0.14.x (4 versions)
  , "0.14.9"
  , "0.14.7"
  , "0.14.5"
  , "0.14.0"
  -- 0.13.x (3 versions)
  , "0.13.8"
  , "0.13.6"
  , "0.13.0"
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
-- | Columns: 0.15.15, 0.15.14, 0.15.10, 0.15.9, 0.15.7, 0.15.4 | 0.14.9, 0.14.7, 0.14.5, 0.14.0 | 0.13.8, 0.13.6, 0.13.0
mockValues :: Array (Array Number)
mockValues =
  --           |-------- 0.15.x --------|  |---- 0.14.x ----|  |-- 0.13.x --|
  [ [1.0, 1.0, 1.0, 1.0, 1.0, 1.0,  1.0, 1.0, 1.0, 1.0,  1.0, 1.0, 1.0]  -- prelude: always works
  , [1.0, 1.0, 1.0, 1.0, 1.0, 1.0,  1.0, 1.0, 1.0, 1.0,  1.0, 1.0, 1.0]  -- effect: always works
  , [1.0, 1.0, 1.0, 1.0, 1.0, 1.0,  1.0, 1.0, 1.0, 1.0,  1.0, 1.0, 1.0]  -- console: always works
  , [1.0, 1.0, 1.0, 1.0, 1.0, 1.0,  1.0, 1.0, 1.0, 1.0,  1.0, 1.0, 1.0]  -- arrays: always works
  , [1.0, 1.0, 1.0, 1.0, 1.0, 1.0,  1.0, 1.0, 1.0, 1.0,  1.0, 1.0, 1.0]  -- maybe: always works
  , [1.0, 1.0, 1.0, 1.0, 1.0, 1.0,  1.0, 1.0, 1.0, 1.0,  1.0, 1.0, 1.0]  -- either: always works
  , [1.0, 1.0, 1.0, 1.0, 1.0, 1.0,  1.0, 1.0, 1.0, 1.0,  1.0, 1.0, 1.0]  -- tuples: always works
  , [1.0, 1.0, 1.0, 1.0, 1.0, 1.0,  1.0, 1.0, 1.0, 1.0,  1.0, 1.0, 1.0]  -- strings: always works
  , [1.0, 1.0, 1.0, 1.0, 1.0, 1.0,  1.0, 1.0, 1.0, 1.0,  1.0, 1.0, 1.0]  -- foldable-traversable
  , [1.0, 1.0, 1.0, 1.0, 1.0, 1.0,  1.0, 1.0, 1.0, 1.0,  1.0, 1.0, 0.5]  -- transformers
  , [1.0, 1.0, 1.0, 1.0, 1.0, 1.0,  1.0, 1.0, 1.0, 0.5,  0.0, 0.0, 0.0]  -- aff: 0.14+ only
  , [1.0, 1.0, 1.0, 1.0, 0.0, 0.0,  0.0, 0.0, 0.0, 0.0,  0.0, 0.0, 0.0]  -- halogen: 0.15.9+ only
  , [1.0, 1.0, 1.0, 1.0, 1.0, 0.5,  1.0, 1.0, 0.5, 0.0,  0.0, 0.0, 0.0]  -- argonaut
  , [1.0, 1.0, 1.0, 1.0, 1.0, 1.0,  1.0, 1.0, 1.0, 1.0,  1.0, 1.0, 1.0]  -- foreign: always works
  , [1.0, 1.0, 1.0, 1.0, 1.0, 0.5,  1.0, 1.0, 0.5, 0.5,  0.0, 0.0, 0.0]  -- node-fs
  , [1.0, 1.0, 1.0, 1.0, 1.0, 0.5,  1.0, 1.0, 0.5, 0.5,  0.0, 0.0, 0.0]  -- node-buffer
  , [1.0, 1.0, 1.0, 0.0, 0.0, 0.0,  0.0, 0.0, 0.0, 0.0,  0.0, 0.0, 0.0]  -- http: very recent
  , [1.0, 1.0, 1.0, 1.0, 1.0, 1.0,  1.0, 1.0, 1.0, 1.0,  1.0, 1.0, 1.0]  -- quickcheck
  , [1.0, 1.0, 1.0, 1.0, 0.5, 0.5,  0.5, 0.0, 0.0, 0.0,  0.0, 0.0, 0.0]  -- spec: recent
  , [1.0, 1.0, 1.0, 1.0, 1.0, 0.5,  1.0, 1.0, 1.0, 0.5,  0.5, 0.0, 0.0]  -- parsing
  , [1.0, 1.0, 0.0, 0.0, 0.0, 0.0,  0.0, 0.0, 0.0, 0.0,  0.0, 0.0, 0.0]  -- routing: very new
  , [1.0, 1.0, 1.0, 1.0, 0.5, 0.5,  0.5, 0.0, 0.0, 0.0,  0.0, 0.0, 0.0]  -- codec: recent
  , [1.0, 1.0, 1.0, 1.0, 1.0, 1.0,  1.0, 1.0, 1.0, 1.0,  1.0, 1.0, 1.0]  -- datetime
  , [1.0, 1.0, 1.0, 1.0, 1.0, 1.0,  1.0, 1.0, 1.0, 1.0,  1.0, 1.0, 0.5]  -- ordered-collections
  , [1.0, 1.0, 1.0, 1.0, 1.0, 1.0,  1.0, 1.0, 1.0, 1.0,  1.0, 0.5, 0.0]  -- profunctor
  , [1.0, 1.0, 1.0, 1.0, 0.5, 0.0,  0.0, 0.0, 0.0, 0.0,  0.0, 0.0, 0.0]  -- variant: recent
  , [1.0, 1.0, 1.0, 1.0, 1.0, 0.5,  1.0, 0.5, 0.0, 0.0,  0.0, 0.0, 0.0]  -- record
  , [1.0, 1.0, 0.0, 0.0, 0.0, 0.0,  0.0, 0.0, 0.0, 0.0,  0.0, 0.0, 0.0]  -- affjax: recent rewrite
  , [1.0, 1.0, 1.0, 0.5, 0.0, 0.0,  0.0, 0.0, 0.0, 0.0,  0.0, 0.0, 0.0]  -- yoga-json: new
  , [1.0, 1.0, 1.0, 1.0, 1.0, 1.0,  1.0, 1.0, 1.0, 1.0,  1.0, 1.0, 1.0]  -- web-dom
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
        { id: pkgIdx * 100 + compIdx  -- Increased multiplier for more compilers
        , package: pkg
        , compiler
        , status
        , timestamp
        , duration
        }
  in
    concat $ mapWithIndex generateJobs mockPackages
