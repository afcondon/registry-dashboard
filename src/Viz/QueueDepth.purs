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
import Data.Maybe (Maybe(..))
import Data.Int (toNumber)
import Data.String (joinWith)
import Effect (Effect)

-- Hylograph HATS Imports
import Hylograph.HATS (Tree, elem, staticStr, staticNum)
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

    scaleY d =
      if maxDepth > 0
      then padding.top + chartHeight - (toNumber d / toNumber maxDepth) * chartHeight
      else padding.top + chartHeight

    -- Build area path
    areaPath = buildAreaPath scaleX scaleY chartHeight padding timeSeries

    -- Build line path (top of area)
    linePath = buildLinePath scaleX scaleY timeSeries

    -- Y-axis ticks
    yTicks = Array.range 0 maxDepth # Array.filter (\n -> n `mod` (max 1 (maxDepth / 4)) == 0)
  in
    elem SVG
      [ staticStr "viewBox" ("0 0 " <> show config.width <> " " <> show config.height)
      , staticStr "width" "100%"
      , staticStr "height" (show config.height)
      , staticStr "preserveAspectRatio" "xMidYMid meet"
      , staticStr "style" "display: block;"
      ]
      [ -- Background
        elem Rect
          [ staticNum "x" padding.left
          , staticNum "y" padding.top
          , staticNum "width" chartWidth
          , staticNum "height" chartHeight
          , staticStr "fill" "#fafafa"
          , staticStr "stroke" "#e0e0e0"
          , staticNum "stroke-width" 1.0
          ]
          []
      , -- Grid lines
        elem Group
          [ staticStr "class" "grid" ]
          (yTicks <#> \tick ->
            elem Line
              [ staticNum "x1" padding.left
              , staticNum "y1" (scaleY tick)
              , staticNum "x2" (padding.left + chartWidth)
              , staticNum "y2" (scaleY tick)
              , staticStr "stroke" "#e8e8e8"
              , staticNum "stroke-width" 1.0
              ]
              []
          )
      , -- Area fill
        elem Path
          [ staticStr "d" areaPath
          , staticStr "fill" "rgba(184, 134, 11, 0.3)"  -- Gold with transparency
          , staticStr "stroke" "none"
          ]
          []
      , -- Line on top
        elem Path
          [ staticStr "d" linePath
          , staticStr "fill" "none"
          , staticStr "stroke" "#b8860b"  -- Gold
          , staticNum "stroke-width" 2.0
          ]
          []
      , -- Y-axis
        elem Line
          [ staticNum "x1" padding.left
          , staticNum "y1" padding.top
          , staticNum "x2" padding.left
          , staticNum "y2" (padding.top + chartHeight)
          , staticStr "stroke" "#666"
          , staticNum "stroke-width" 1.0
          ]
          []
      , -- Y-axis labels
        elem Group
          [ staticStr "class" "y-labels" ]
          (yTicks <#> \tick ->
            elem Text
              [ staticNum "x" (padding.left - 8.0)
              , staticNum "y" (scaleY tick + 4.0)
              , staticStr "text-anchor" "end"
              , staticStr "font-size" "11"
              , staticStr "fill" "#666"
              , staticStr "textContent" (show tick)
              ]
              []
          )
      , -- X-axis
        elem Line
          [ staticNum "x1" padding.left
          , staticNum "y1" (padding.top + chartHeight)
          , staticNum "x2" (padding.left + chartWidth)
          , staticNum "y2" (padding.top + chartHeight)
          , staticStr "stroke" "#666"
          , staticNum "stroke-width" 1.0
          ]
          []
      , -- X-axis labels
        elem Text
          [ staticNum "x" padding.left
          , staticNum "y" (config.height - 5.0)
          , staticStr "font-size" "11"
          , staticStr "fill" "#666"
          , staticStr "textContent" "← Earlier"
          ]
          []
      , elem Text
          [ staticNum "x" (padding.left + chartWidth)
          , staticNum "y" (config.height - 5.0)
          , staticStr "text-anchor" "end"
          , staticStr "font-size" "11"
          , staticStr "fill" "#666"
          , staticStr "textContent" "Later →"
          ]
          []
      , -- Y-axis title
        elem Text
          [ staticNum "x" 12.0
          , staticNum "y" (padding.top + chartHeight / 2.0)
          , staticStr "font-size" "11"
          , staticStr "fill" "#666"
          , staticStr "transform" ("rotate(-90, 12, " <> show (padding.top + chartHeight / 2.0) <> ")")
          , staticStr "text-anchor" "middle"
          , staticStr "textContent" "Queue Depth"
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
          let x = scaleX pt.time
              y = scaleY pt.depth
          in "L " <> show x <> " " <> show y

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
