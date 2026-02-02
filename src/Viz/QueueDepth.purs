-- | Viz.QueueDepth
-- |
-- | HATS-based area chart showing job queue depth over time.
-- | Visualizes build queue pressure on the registry.
module Viz.QueueDepth
  ( render
  , cleanup
  , Config
  , defaultConfig
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Effect (Effect)

-- Hylograph HATS Imports
import Hylograph.HATS (Tree, elem)
import Hylograph.HATS.Friendly (attr, x, y, x1, y1, x2, y2, width, height, fill, stroke, strokeWidth, d, class_, style, textAnchor, fontSize, transform)
import Hylograph.HATS.InterpreterTick (rerender, clearContainer)
import Hylograph.Internal.Selection.Types (ElementType(..))

import Data.Types (MatrixJob)

-- =============================================================================
-- Types
-- =============================================================================

type Config =
  { containerSelector :: String
  , width :: Number
  , height :: Number
  }

defaultConfig :: Config
defaultConfig =
  { containerSelector: "#queue-depth-container"
  , width: 800.0
  , height: 150.0
  }

type TimePoint =
  { time :: Number
  , depth :: Int
  }

-- =============================================================================
-- Public API
-- =============================================================================

render :: Config -> Array MatrixJob -> Effect Unit
render config jobs = do
  clearContainer config.containerSelector
  let timeSeries = computeQueueDepth jobs
      tree = buildChartTree config timeSeries
  _ <- rerender config.containerSelector tree
  pure unit

cleanup :: String -> Effect Unit
cleanup = clearContainer

-- =============================================================================
-- Queue Depth Computation
-- =============================================================================

-- | Compute queue depth over time from job data
-- | Simulates jobs arriving and completing based on timestamp and duration
computeQueueDepth :: Array MatrixJob -> Array TimePoint
computeQueueDepth jobs =
  let
    -- Create events for job start and end
    events = jobs >>= \job ->
      [ { time: job.timestamp - job.duration * 1000.0, delta: 1 }  -- Job started
      , { time: job.timestamp, delta: -1 }                          -- Job completed
      ]

    -- Sort events by time
    sortedEvents = Array.sortBy (\a b -> compare a.time b.time) events

    -- Accumulate queue depth
    accumulate :: { depth :: Int, points :: Array TimePoint } -> { time :: Number, delta :: Int } -> { depth :: Int, points :: Array TimePoint }
    accumulate acc event =
      let newDepth = acc.depth + event.delta
          point = { time: event.time, depth: newDepth }
      in { depth: newDepth, points: Array.snoc acc.points point }

    result = foldl accumulate { depth: 0, points: [] } sortedEvents
  in
    result.points

-- =============================================================================
-- HATS Rendering
-- =============================================================================

buildChartTree :: Config -> Array TimePoint -> Tree
buildChartTree config timeSeries =
  let
    padding = { top: 20.0, right: 40.0, bottom: 30.0, left: 50.0 }
    chartWidth = config.width - padding.left - padding.right
    chartHeight = config.height - padding.top - padding.bottom

    -- Compute ranges
    times = timeSeries <#> _.time
    depths = timeSeries <#> _.depth
    minTime = Array.foldl min 9999999999999.0 times
    maxTime = Array.foldl max 0.0 times
    maxDepth = Array.foldl max 0 depths

    -- Scale functions
    scaleX t =
      if maxTime > minTime
      then padding.left + ((t - minTime) / (maxTime - minTime)) * chartWidth
      else padding.left + chartWidth / 2.0

    scaleY dep =
      if maxDepth > 0
      then padding.top + chartHeight - (toNumber dep / toNumber maxDepth) * chartHeight
      else padding.top + chartHeight

    -- Build area path
    areaPath = buildAreaPath scaleX scaleY chartHeight padding timeSeries

    -- Build line path (top of area)
    linePath = buildLinePath scaleX scaleY timeSeries

    -- Y-axis ticks
    yTicks = Array.range 0 maxDepth # Array.filter (\n -> n `mod` (max 1 (maxDepth / 4)) == 0)

    vbStr = "0 0 " <> show config.width <> " " <> show config.height
  in
    elem SVG
      [ attr "viewBox" vbStr
      , attr "width" "100%"
      , height config.height
      , attr "preserveAspectRatio" "xMidYMid meet"
      , style "display: block;"
      ]
      [ -- Background
        elem Rect
          [ x padding.left
          , y padding.top
          , width chartWidth
          , height chartHeight
          , fill "#fafafa"
          , stroke "#e0e0e0"
          , strokeWidth 1.0
          ]
          []
      , -- Grid lines
        elem Group
          [ class_ "grid" ]
          (yTicks <#> \tick ->
            elem Line
              [ x1 padding.left
              , y1 (scaleY tick)
              , x2 (padding.left + chartWidth)
              , y2 (scaleY tick)
              , stroke "#e8e8e8"
              , strokeWidth 1.0
              ]
              []
          )
      , -- Area fill
        elem Path
          [ d areaPath
          , fill "rgba(184, 134, 11, 0.3)"  -- Gold with transparency
          , stroke "none"
          ]
          []
      , -- Line on top
        elem Path
          [ d linePath
          , fill "none"
          , stroke "#b8860b"  -- Gold
          , strokeWidth 2.0
          ]
          []
      , -- Y-axis
        elem Line
          [ x1 padding.left
          , y1 padding.top
          , x2 padding.left
          , y2 (padding.top + chartHeight)
          , stroke "#666"
          , strokeWidth 1.0
          ]
          []
      , -- Y-axis labels
        elem Group
          [ class_ "y-labels" ]
          (yTicks <#> \tick ->
            elem Text
              [ x (padding.left - 8.0)
              , y (scaleY tick + 4.0)
              , textAnchor "end"
              , fontSize "11"
              , fill "#666"
              , attr "textContent" (show tick)
              ]
              []
          )
      , -- X-axis
        elem Line
          [ x1 padding.left
          , y1 (padding.top + chartHeight)
          , x2 (padding.left + chartWidth)
          , y2 (padding.top + chartHeight)
          , stroke "#666"
          , strokeWidth 1.0
          ]
          []
      , -- X-axis labels
        elem Text
          [ x padding.left
          , y (config.height - 5.0)
          , fontSize "11"
          , fill "#666"
          , attr "textContent" "← Earlier"
          ]
          []
      , elem Text
          [ x (padding.left + chartWidth)
          , y (config.height - 5.0)
          , textAnchor "end"
          , fontSize "11"
          , fill "#666"
          , attr "textContent" "Later →"
          ]
          []
      , -- Y-axis title
        elem Text
          [ x 12.0
          , y (padding.top + chartHeight / 2.0)
          , fontSize "11"
          , fill "#666"
          , transform ("rotate(-90, 12, " <> show (padding.top + chartHeight / 2.0) <> ")")
          , textAnchor "middle"
          , attr "textContent" "Queue Depth"
          ]
          []
      ]

-- | Build SVG path for the filled area
buildAreaPath :: (Number -> Number) -> (Int -> Number) -> Number -> { top :: Number, right :: Number, bottom :: Number, left :: Number } -> Array TimePoint -> String
buildAreaPath scaleX scaleY chartHeight padding points =
  case Array.uncons points of
    Nothing -> ""
    Just { head, tail } ->
      let
        startX = scaleX head.time
        startY = scaleY head.depth
        baseline = padding.top + chartHeight

        -- Move to start, line to first point
        start = "M " <> show startX <> " " <> show baseline <> " L " <> show startX <> " " <> show startY

        -- Line through all points (step function for queue depth)
        lineSegments = tail <#> \pt ->
          let ptX = scaleX pt.time
              ptY = scaleY pt.depth
          in "L " <> show ptX <> " " <> show ptY

        -- Close back to baseline
        lastPoint = Array.last points
        endX = case lastPoint of
          Nothing -> startX
          Just pt -> scaleX pt.time
        close = " L " <> show endX <> " " <> show baseline <> " Z"
      in
        start <> " " <> joinWith " " lineSegments <> close

-- | Build SVG path for the line only
buildLinePath :: (Number -> Number) -> (Int -> Number) -> Array TimePoint -> String
buildLinePath scaleX scaleY points =
  case Array.uncons points of
    Nothing -> ""
    Just { head, tail } ->
      let
        start = "M " <> show (scaleX head.time) <> " " <> show (scaleY head.depth)
        segments = tail <#> \pt ->
          "L " <> show (scaleX pt.time) <> " " <> show (scaleY pt.depth)
      in
        start <> " " <> joinWith " " segments
