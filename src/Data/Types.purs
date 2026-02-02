-- | Data.Types
-- |
-- | Domain types for the Registry Dashboard.
-- | Defines structures for compiler compatibility matrix visualization.
module Data.Types
  ( PackageName
  , CompilerVersion
  , MajorVersion
  , CompatibilityStatus(..)
  , CompatibilityMatrix
  , MatrixJob
  , CompilerGroup
  , statusToNumber
  , numberToStatus
  , getMajorVersion
  , groupCompilers
  , aggregateStatus
  ) where

import Prelude

import Data.Array (filter, foldl, groupBy, length)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.String (take)

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

-- | Major version string (e.g., "0.15", "0.14")
type MajorVersion = String

-- | A group of compiler versions under a major version
type CompilerGroup =
  { major :: MajorVersion
  , versions :: Array CompilerVersion
  , indices :: Array Int  -- Original indices in the matrix
  }

-- | Extract major version from compiler version string
-- | "0.15.10" -> "0.15"
getMajorVersion :: CompilerVersion -> MajorVersion
getMajorVersion version = take 4 version  -- "0.15" is 4 chars

-- | Group compilers by major version, preserving original indices
groupCompilers :: Array CompilerVersion -> Array CompilerGroup
groupCompilers compilers =
  let
    -- Pair each compiler with its index
    indexed = compilers # foldl (\acc c -> acc <> [{ version: c, idx: length acc }]) []

    -- Group by major version
    grouped :: Array (NonEmptyArray { version :: CompilerVersion, idx :: Int })
    grouped = groupBy (\a b -> getMajorVersion a.version == getMajorVersion b.version) indexed

    -- Convert to CompilerGroup
    toGroup :: NonEmptyArray { version :: CompilerVersion, idx :: Int } -> CompilerGroup
    toGroup items =
      let arr = NEA.toArray items
          major = getMajorVersion (NEA.head items).version
      in { major
         , versions: arr <#> _.version
         , indices: arr <#> _.idx
         }
  in
    grouped <#> toGroup

-- | Aggregate status values for a group (for collapsed view)
-- | Returns: 1.0 if all pass, 0.0 if any fail, 0.5 if mixed/untested
aggregateStatus :: Array Number -> Number
aggregateStatus values =
  let
    passing = filter (_ >= 0.75) values
    failing = filter (_ <= 0.25) values
    total = length values
    numPassing = length passing
    numFailing = length failing
  in
    if numPassing == total then 1.0
    else if numFailing > 0 then 0.0
    else 0.5
