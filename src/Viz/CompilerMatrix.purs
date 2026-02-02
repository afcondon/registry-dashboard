-- | Viz.CompilerMatrix
-- |
-- | HATS-based rendering for the compiler compatibility matrix.
-- | Uses Hylograph for type-safe SVG generation.
module Viz.CompilerMatrix
  ( render
  , Config
  , defaultConfig
  ) where

import Prelude

import Data.Array (length, mapWithIndex, (!!))
import Data.Array as Array
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Effect (Effect)

-- Hylograph HATS Imports
import Hylograph.HATS (Tree, elem, staticStr, staticNum)
import Hylograph.HATS.InterpreterTick (rerender)
import Hylograph.Internal.Selection.Types (ElementType(..))

-- Hylograph Scale for colors
import Hylograph.Scale (interpolateRdYlGn)

import Data.Types (CompatibilityMatrix)

-- =============================================================================
-- Configuration
-- =============================================================================

type Config =
  { containerSelector :: String
  , cellWidth :: Number
  , cellHeight :: Number
  , rowLabelWidth :: Number
  , colLabelHeight :: Number
  , padding :: Number
  }

defaultConfig :: Config
defaultConfig =
  { containerSelector: "#matrix-container"
  , cellWidth: 50.0
  , cellHeight: 20.0
  , rowLabelWidth: 150.0
  , colLabelHeight: 80.0
  , padding: 8.0
  }

-- =============================================================================
-- Internal Types
-- =============================================================================

type MatrixCell =
  { row :: Int
  , col :: Int
  , value :: Number
  , packageName :: String
  , compilerVersion :: String
  , x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  , color :: String
  }

type RowLabel =
  { index :: Int
  , name :: String
  , x :: Number
  , y :: Number
  }

type ColLabel =
  { index :: Int
  , name :: String
  , x :: Number
  , y :: Number
  }

type MatrixLayout =
  { cells :: Array MatrixCell
  , rowLabels :: Array RowLabel
  , colLabels :: Array ColLabel
  , totalWidth :: Number
  , totalHeight :: Number
  , gridWidth :: Number
  , gridHeight :: Number
  , gridOffsetX :: Number
  , gridOffsetY :: Number
  }

-- =============================================================================
-- Public API
-- =============================================================================

-- | Render the compatibility matrix to the specified container
render :: Config -> CompatibilityMatrix -> Effect Unit
render config matrix = do
  let layout = computeLayout config matrix
      tree = buildMatrixTree config layout
  _ <- rerender config.containerSelector tree
  pure unit

-- =============================================================================
-- Layout Computation
-- =============================================================================

computeLayout :: Config -> CompatibilityMatrix -> MatrixLayout
computeLayout config matrix =
  let
    numRows = length matrix.packages
    numCols = length matrix.compilers

    gridWidth = config.cellWidth * toNumber numCols
    gridHeight = config.cellHeight * toNumber numRows

    gridOffsetX = config.rowLabelWidth + config.padding
    gridOffsetY = config.colLabelHeight + config.padding

    totalWidth = gridOffsetX + gridWidth + config.padding
    totalHeight = gridOffsetY + gridHeight + config.padding

    cells = buildCells config gridOffsetX gridOffsetY matrix
    rowLabels = buildRowLabels config gridOffsetY matrix.packages
    colLabels = buildColLabels config gridOffsetX matrix.compilers
  in
    { cells
    , rowLabels
    , colLabels
    , totalWidth
    , totalHeight
    , gridWidth
    , gridHeight
    , gridOffsetX
    , gridOffsetY
    }

buildCells :: Config -> Number -> Number -> CompatibilityMatrix -> Array MatrixCell
buildCells config offsetX offsetY matrix =
  Array.concat $ mapWithIndex buildRow matrix.values
  where
  buildRow :: Int -> Array Number -> Array MatrixCell
  buildRow rowIdx row = mapWithIndex (buildCell rowIdx) row

  buildCell :: Int -> Int -> Number -> MatrixCell
  buildCell rowIdx colIdx value =
    let
      packageName = fromMaybe "" $ matrix.packages !! rowIdx
      compilerVersion = fromMaybe "" $ matrix.compilers !! colIdx
      x = offsetX + toNumber colIdx * config.cellWidth
      y = offsetY + toNumber rowIdx * config.cellHeight
    in
      { row: rowIdx
      , col: colIdx
      , value
      , packageName
      , compilerVersion
      , x
      , y
      , width: config.cellWidth
      , height: config.cellHeight
      , color: interpolateRdYlGn value
      }

buildRowLabels :: Config -> Number -> Array String -> Array RowLabel
buildRowLabels config gridOffsetY packages = mapWithIndex buildLabel packages
  where
  buildLabel :: Int -> String -> RowLabel
  buildLabel idx name =
    { index: idx
    , name
    , x: config.rowLabelWidth - config.padding
    , y: gridOffsetY + toNumber idx * config.cellHeight + config.cellHeight / 2.0
    }

buildColLabels :: Config -> Number -> Array String -> Array ColLabel
buildColLabels config gridOffsetX compilers = mapWithIndex buildLabel compilers
  where
  buildLabel :: Int -> String -> ColLabel
  buildLabel idx name =
    { index: idx
    , name
    , x: gridOffsetX + toNumber idx * config.cellWidth + config.cellWidth / 2.0
    , y: config.colLabelHeight - config.padding
    }

-- =============================================================================
-- HATS Tree Building
-- =============================================================================

buildMatrixTree :: Config -> MatrixLayout -> Tree
buildMatrixTree _config layout =
  let
    viewBox = "0 0 " <> show layout.totalWidth <> " " <> show layout.totalHeight
  in
    elem SVG
      [ staticStr "id" "matrix-svg"
      , staticStr "viewBox" viewBox
      , staticStr "width" (show layout.totalWidth)
      , staticStr "height" (show layout.totalHeight)
      , staticStr "style" "background: transparent; display: block;"
      ]
      [ -- Grid background (light theme)
        elem Rect
          [ staticNum "x" layout.gridOffsetX
          , staticNum "y" layout.gridOffsetY
          , staticNum "width" layout.gridWidth
          , staticNum "height" layout.gridHeight
          , staticStr "fill" "#f8f8f8"
          , staticStr "stroke" "#e0e0e0"
          ] []
      , -- Cells group
        elem Group
          [ staticStr "class" "cells" ]
          (layout.cells <#> renderCell)
      , -- Row labels group
        elem Group
          [ staticStr "class" "row-labels" ]
          (layout.rowLabels <#> renderRowLabel)
      , -- Column labels group
        elem Group
          [ staticStr "class" "col-labels" ]
          (layout.colLabels <#> renderColLabel)
      ]

renderCell :: MatrixCell -> Tree
renderCell cell =
  elem Rect
    [ staticStr "class" "matrix-cell"
    , staticNum "x" cell.x
    , staticNum "y" cell.y
    , staticNum "width" (cell.width - 2.0)
    , staticNum "height" (cell.height - 2.0)
    , staticStr "fill" cell.color
    , staticNum "rx" 2.0
    , staticStr "data-package" cell.packageName
    , staticStr "data-compiler" cell.compilerVersion
    , staticNum "data-value" cell.value
    ] []

renderRowLabel :: RowLabel -> Tree
renderRowLabel label =
  elem Text
    [ staticStr "class" "row-label"
    , staticNum "x" label.x
    , staticNum "y" label.y
    , staticStr "text-anchor" "end"
    , staticStr "dominant-baseline" "middle"
    , staticStr "fill" "#333333"
    , staticStr "font-size" "11px"
    , staticStr "font-family" "ui-monospace, SFMono-Regular, Menlo, Monaco, monospace"
    , staticStr "textContent" label.name
    ] []

renderColLabel :: ColLabel -> Tree
renderColLabel label =
  let
    transform = "rotate(-45," <> show label.x <> "," <> show label.y <> ")"
  in
    elem Text
      [ staticStr "class" "col-label"
      , staticNum "x" label.x
      , staticNum "y" label.y
      , staticStr "text-anchor" "start"
      , staticStr "dominant-baseline" "middle"
      , staticStr "transform" transform
      , staticStr "fill" "#333333"
      , staticStr "font-size" "11px"
      , staticStr "font-family" "ui-monospace, SFMono-Regular, Menlo, Monaco, monospace"
      , staticStr "textContent" label.name
      ] []
