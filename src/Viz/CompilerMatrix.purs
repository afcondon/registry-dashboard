-- | Viz.CompilerMatrix
-- |
-- | HATS-based rendering for the compiler compatibility matrix.
-- | Supports collapsible column groups by major version.
-- | State is managed externally (by Halogen component).
module Viz.CompilerMatrix
  ( render
  , setupClickHandlers
  , Config
  , defaultConfig
  ) where

import Prelude

import Data.Array (concat, length, mapWithIndex, (!!))
import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)

-- Hylograph HATS Imports
import Hylograph.HATS (Tree, elem)
import Hylograph.HATS.Friendly (attr, attrNum, viewBox, x, y, width, height, fill, stroke, class_, style, transform, textAnchor, fontSize, fontFamily)
import Hylograph.HATS.InterpreterTick (rerender)
import Hylograph.Internal.Selection.Types (ElementType(..))

-- Hylograph Scale for colors
import Hylograph.Scale (interpolateRdYlGn)

import Data.Types (CompatibilityMatrix, MajorVersion, CompilerGroup, groupCompilers, aggregateStatus)

-- FFI for click handlers
foreign import addGroupClickHandlers :: String -> (MajorVersion -> Effect Unit) -> Effect Unit

-- =============================================================================
-- Configuration
-- =============================================================================

type Config =
  { containerSelector :: String
  , cellWidth :: Number
  , cellHeight :: Number
  , groupHeaderWidth :: Number
  , rowLabelWidth :: Number
  , colLabelHeight :: Number
  , padding :: Number
  }

defaultConfig :: Config
defaultConfig =
  { containerSelector: "#matrix-container"
  , cellWidth: 40.0
  , cellHeight: 20.0
  , groupHeaderWidth: 60.0
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

type GroupHeader =
  { major :: MajorVersion
  , x :: Number
  , y :: Number
  , width :: Number
  , expanded :: Boolean
  , versionCount :: Int
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
  , visible :: Boolean
  }

type MatrixLayout =
  { cells :: Array MatrixCell
  , rowLabels :: Array RowLabel
  , colLabels :: Array ColLabel
  , groupHeaders :: Array GroupHeader
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

-- | Render the matrix with given expanded state (state owned by caller)
render :: Config -> CompatibilityMatrix -> Set MajorVersion -> Effect Unit
render config matrix expanded = do
  let groups = groupCompilers matrix.compilers
      layout = computeLayout config matrix groups expanded
      tree = buildMatrixTree config layout
  _ <- rerender config.containerSelector tree
  pure unit

-- | Set up click handlers that call back when a group header is clicked
setupClickHandlers :: Config -> (MajorVersion -> Effect Unit) -> Effect Unit
setupClickHandlers config callback =
  addGroupClickHandlers config.containerSelector callback

-- =============================================================================
-- Layout Computation
-- =============================================================================

computeLayout :: Config -> CompatibilityMatrix -> Array CompilerGroup -> Set MajorVersion -> MatrixLayout
computeLayout config matrix groups expanded =
  let
    numRows = length matrix.packages

    gridWidth = foldl (\acc g ->
      if Set.member g.major expanded
      then acc + toNumber (length g.versions) * config.cellWidth
      else acc + config.groupHeaderWidth
    ) 0.0 groups

    gridHeight = config.cellHeight * toNumber numRows

    gridOffsetX = config.rowLabelWidth + config.padding
    gridOffsetY = config.colLabelHeight + config.padding

    totalWidth = gridOffsetX + gridWidth + config.padding
    totalHeight = gridOffsetY + gridHeight + config.padding

    columnsInfo = buildColumnsInfo config groups expanded gridOffsetX

    cells = buildCells config gridOffsetY matrix groups expanded columnsInfo
    rowLabels = buildRowLabels config gridOffsetY matrix.packages
    colLabels = buildColLabels config columnsInfo matrix.compilers
    groupHeaders = buildGroupHeaders config gridOffsetY groups expanded columnsInfo
  in
    { cells
    , rowLabels
    , colLabels
    , groupHeaders
    , totalWidth
    , totalHeight
    , gridWidth
    , gridHeight
    , gridOffsetX
    , gridOffsetY
    }

type ColumnInfo =
  { compilerIdx :: Int
  , x :: Number
  , visible :: Boolean
  , groupMajor :: MajorVersion
  }

buildColumnsInfo :: Config -> Array CompilerGroup -> Set MajorVersion -> Number -> Array ColumnInfo
buildColumnsInfo config groups expanded startX =
  let
    buildGroupColumns :: { x :: Number, cols :: Array ColumnInfo } -> CompilerGroup -> { x :: Number, cols :: Array ColumnInfo }
    buildGroupColumns acc group =
      let isExpanded = Set.member group.major expanded
      in if isExpanded
         then
           let newCols = group.indices # mapWithIndex \i idx ->
                 { compilerIdx: idx
                 , x: acc.x + toNumber i * config.cellWidth
                 , visible: true
                 , groupMajor: group.major
                 }
               newX = acc.x + toNumber (length group.indices) * config.cellWidth
           in { x: newX, cols: acc.cols <> newCols }
         else
           let newCols = group.indices # mapWithIndex \_ idx ->
                 { compilerIdx: idx
                 , x: acc.x
                 , visible: false
                 , groupMajor: group.major
                 }
               newX = acc.x + config.groupHeaderWidth
           in { x: newX, cols: acc.cols <> newCols }
  in
    (foldl buildGroupColumns { x: startX, cols: [] } groups).cols

buildCells :: Config -> Number -> CompatibilityMatrix -> Array CompilerGroup -> Set MajorVersion -> Array ColumnInfo -> Array MatrixCell
buildCells config offsetY matrix groups expanded columnsInfo =
  concat $ mapWithIndex buildRow matrix.values
  where
  buildRow :: Int -> Array Number -> Array MatrixCell
  buildRow rowIdx row =
    concat $ groups <#> \group ->
      let isExpanded = Set.member group.major expanded
          groupValues = group.indices # Array.mapMaybe \idx -> row !! idx
      in if isExpanded
         then group.indices # mapWithIndex \_ idx ->
           let value = fromMaybe 0.5 $ row !! idx
               colInfo = Array.find (\c -> c.compilerIdx == idx) columnsInfo
               cellX = fromMaybe 0.0 $ colInfo <#> _.x
           in { row: rowIdx
              , col: idx
              , value
              , packageName: fromMaybe "" $ matrix.packages !! rowIdx
              , compilerVersion: fromMaybe "" $ matrix.compilers !! idx
              , x: cellX
              , y: offsetY + toNumber rowIdx * config.cellHeight
              , width: config.cellWidth
              , height: config.cellHeight
              , color: interpolateRdYlGn value
              }
         else
           let aggValue = aggregateStatus groupValues
               colInfo = Array.find (\c -> c.groupMajor == group.major) columnsInfo
               cellX = fromMaybe 0.0 $ colInfo <#> _.x
           in [{ row: rowIdx
               , col: -1
               , value: aggValue
               , packageName: fromMaybe "" $ matrix.packages !! rowIdx
               , compilerVersion: group.major <> ".x"
               , x: cellX
               , y: offsetY + toNumber rowIdx * config.cellHeight
               , width: config.groupHeaderWidth
               , height: config.cellHeight
               , color: interpolateRdYlGn aggValue
               }]

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

buildColLabels :: Config -> Array ColumnInfo -> Array String -> Array ColLabel
buildColLabels config columnsInfo compilers =
  columnsInfo # Array.mapMaybe \colInfo ->
    if colInfo.visible
    then Just
      { index: colInfo.compilerIdx
      , name: fromMaybe "" $ compilers !! colInfo.compilerIdx
      , x: colInfo.x + config.cellWidth / 2.0
      , y: config.colLabelHeight - config.padding
      , visible: true
      }
    else Nothing

buildGroupHeaders :: Config -> Number -> Array CompilerGroup -> Set MajorVersion -> Array ColumnInfo -> Array GroupHeader
buildGroupHeaders config gridOffsetY groups expanded columnsInfo =
  groups <#> \group ->
    let isExpanded = Set.member group.major expanded
        colInfo = Array.find (\c -> c.groupMajor == group.major) columnsInfo
        headerX = fromMaybe 0.0 $ colInfo <#> _.x
        headerWidth = if isExpanded
                      then toNumber (length group.versions) * config.cellWidth
                      else config.groupHeaderWidth
    in { major: group.major
       , x: headerX
       , y: gridOffsetY - 35.0
       , width: headerWidth
       , expanded: isExpanded
       , versionCount: length group.versions
       }

-- =============================================================================
-- HATS Tree Building
-- =============================================================================

buildMatrixTree :: Config -> MatrixLayout -> Tree
buildMatrixTree _config layout =
  elem SVG
    [ attr "id" "matrix-svg"
    , viewBox 0.0 0.0 layout.totalWidth layout.totalHeight
    , width layout.totalWidth
    , height layout.totalHeight
    , style "background: transparent; display: block;"
    ]
    [ elem Rect
        [ x layout.gridOffsetX
        , y layout.gridOffsetY
        , width layout.gridWidth
        , height layout.gridHeight
        , fill "#f8f8f8"
        , stroke "#e0e0e0"
        ] []
    , elem Group
        [ class_ "group-headers" ]
        (layout.groupHeaders <#> renderGroupHeader)
    , elem Group
        [ class_ "cells" ]
        (layout.cells <#> renderCell)
    , elem Group
        [ class_ "row-labels" ]
        (layout.rowLabels <#> renderRowLabel)
    , elem Group
        [ class_ "col-labels" ]
        (layout.colLabels <#> renderColLabel)
    ]

renderGroupHeader :: GroupHeader -> Tree
renderGroupHeader header =
  let
    icon = if header.expanded then "▼" else "▶"
    label = header.major <> " (" <> show header.versionCount <> ")"
  in
    elem Group
      [ class_ "group-header"
      , attr "data-major" header.major
      , style "cursor: pointer;"
      ]
      [ elem Rect
          [ x header.x
          , y header.y
          , width header.width
          , height 30.0
          , fill (if header.expanded then "#e8e8e8" else "#f0f0f0")
          , stroke "#ccc"
          , attrNum "rx" 4.0
          ] []
      , elem Text
          [ x (header.x + header.width / 2.0)
          , y (header.y + 20.0)
          , textAnchor "middle"
          , fontSize "12px"
          , fontFamily "system-ui, sans-serif"
          , fill "#333"
          , attr "textContent" (icon <> " " <> label)
          ] []
      ]

renderCell :: MatrixCell -> Tree
renderCell cell =
  elem Rect
    [ class_ "matrix-cell"
    , x cell.x
    , y cell.y
    , width (cell.width - 2.0)
    , height (cell.height - 2.0)
    , fill cell.color
    , attrNum "rx" 2.0
    , attr "data-package" cell.packageName
    , attr "data-compiler" cell.compilerVersion
    , attrNum "data-value" cell.value
    ] []

renderRowLabel :: RowLabel -> Tree
renderRowLabel label =
  elem Text
    [ class_ "row-label"
    , x label.x
    , y label.y
    , textAnchor "end"
    , attr "dominant-baseline" "middle"
    , fill "#333333"
    , fontSize "11px"
    , fontFamily "ui-monospace, SFMono-Regular, Menlo, Monaco, monospace"
    , attr "textContent" label.name
    ] []

renderColLabel :: ColLabel -> Tree
renderColLabel label =
  let
    rotateTransform = "rotate(-45," <> show label.x <> "," <> show label.y <> ")"
  in
    elem Text
      [ class_ "col-label"
      , x label.x
      , y label.y
      , textAnchor "start"
      , attr "dominant-baseline" "middle"
      , transform rotateTransform
      , fill "#333333"
      , fontSize "11px"
      , fontFamily "ui-monospace, SFMono-Regular, Menlo, Monaco, monospace"
      , attr "textContent" label.name
      ] []
