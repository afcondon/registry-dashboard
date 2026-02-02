-- | Data.Types
-- |
-- | Domain types for the Registry Dashboard.
-- | Defines structures for compiler compatibility matrix visualization.
module Data.Types
  ( PackageName
  , CompilerVersion
  , CompatibilityStatus(..)
  , CompatibilityMatrix
  , MatrixJob
  , statusToNumber
  , numberToStatus
  ) where

import Prelude

-- | Package name from the registry (e.g., "prelude", "effect")
type PackageName = String

-- | PureScript compiler version (e.g., "0.15.10", "0.15.9")
type CompilerVersion = String

-- | The compatibility status of a package with a compiler version
data CompatibilityStatus
  = Compatible    -- ^ Package builds successfully
  | Failed        -- ^ Package fails to build
  | Untested      -- ^ No matrix job has been run

derive instance eqCompatibilityStatus :: Eq CompatibilityStatus

instance showCompatibilityStatus :: Show CompatibilityStatus where
  show Compatible = "Compatible"
  show Failed = "Failed"
  show Untested = "Untested"

-- | Convert status to a number for visualization (0.0-1.0 scale)
statusToNumber :: CompatibilityStatus -> Number
statusToNumber Compatible = 1.0
statusToNumber Failed = 0.0
statusToNumber Untested = 0.5

-- | Convert a number back to status (for legend/tooltip)
numberToStatus :: Number -> CompatibilityStatus
numberToStatus n
  | n >= 0.75 = Compatible
  | n <= 0.25 = Failed
  | otherwise = Untested

-- | The full compatibility matrix structure
type CompatibilityMatrix =
  { packages :: Array PackageName      -- Row labels
  , compilers :: Array CompilerVersion -- Column labels
  , values :: Array (Array Number)     -- 2D matrix: packages x compilers
  }

-- | A single matrix job result (for future API integration)
type MatrixJob =
  { id :: Int
  , package :: PackageName
  , compiler :: CompilerVersion
  , status :: CompatibilityStatus
  , timestamp :: Number  -- Unix timestamp in ms
  , duration :: Number   -- Job duration in seconds
  }
