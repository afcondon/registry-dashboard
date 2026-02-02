-- | Viz.JobBeeswarm
-- |
-- | HATS-based beeswarm visualization for matrix job timeline.
-- | Uses force simulation to spread jobs vertically while positioning by time.
module Viz.JobBeeswarm
  ( render
  , cleanup
  , Config
  , defaultConfig
  , BeeswarmHandle
  ) where

import Prelude

import Data.Array as Array
import Data.Int (toNumber)
import Data.Nullable as Nullable
import Data.Number (sqrt)
import Effect (Effect)

-- Hylograph HATS Imports
import Hylograph.HATS (Tree, elem, thunkedStr, thunkedNum, forEach)
import Hylograph.HATS.Friendly (attr, x1, y1, x2, y2, x, y, stroke, strokeWidth, fill, class_, style, textAnchor, fontSize, opacity)
import Hylograph.HATS.InterpreterTick (rerender, clearContainer)
import Hylograph.Internal.Selection.Types (ElementType(..))
import Hylograph.Simulation.HATS (tickUpdate)

-- Simulation imports
import Hylograph.Simulation
  ( runSimulation
  , Engine(..)
  , SimulationEvent(..)
  , SimulationHandle
  , subscribe
  , setup
  , collide
  , positionX
  , positionY
  , withStrength
  , withRadius
  , withX
  , withY
  , static
  , dynamic
  )
import Hylograph.ForceEngine.Simulation (SimulationNode)
import Hylograph.ForceEngine.Setup (withAlphaDecay)

import Data.Types (MatrixJob, CompatibilityStatus(..))

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
  { containerSelector: "#beeswarm-container"
  , width: 800.0
  , height: 200.0
  }

-- | Job as a simulation node
type JobNode = SimulationNode
  ( job :: MatrixJob
  , targetX :: Number
  , r :: Number
  , color :: String
  )

type JobNodeRow =
  ( job :: MatrixJob
  , targetX :: Number
  , r :: Number
  , color :: String
  )

type BeeswarmHandle =
  { stop :: Effect Unit
  }

-- =============================================================================
-- Public API
-- =============================================================================

render :: Config -> Array MatrixJob -> Effect BeeswarmHandle
render config jobs = do
  clearContainer config.containerSelector

  -- Compute time range
  let timeRange = computeTimeRange jobs
      nodes = prepareNodes config timeRange jobs

  -- Create SVG container
  renderSVGContainer config

  -- Start simulation
  simHandle <- startSimulation config nodes

  pure { stop: simHandle.stop }

cleanup :: String -> Effect Unit
cleanup = clearContainer

-- =============================================================================
-- Node Preparation
-- =============================================================================

type TimeRange = { minTime :: Number, maxTime :: Number }

computeTimeRange :: Array MatrixJob -> TimeRange
computeTimeRange jobs =
  let
    times = jobs <#> _.timestamp
    minTime = Array.foldl min 9999999999999.0 times
    maxTime = Array.foldl max 0.0 times
  in
    { minTime, maxTime }

prepareNodes :: Config -> TimeRange -> Array MatrixJob -> Array JobNode
prepareNodes config timeRange jobs =
  Array.mapWithIndex (prepareNode config timeRange) jobs

prepareNode :: Config -> TimeRange -> Int -> MatrixJob -> JobNode
prepareNode config timeRange idx job =
  let
    padding = 60.0
    availableWidth = config.width - padding * 2.0

    -- Map timestamp to x position
    range = timeRange.maxTime - timeRange.minTime
    t = if range > 0.0
          then (job.timestamp - timeRange.minTime) / range
          else 0.5
    targetX = padding + t * availableWidth - config.width / 2.0

    -- Radius based on job duration (longer jobs = bigger circles)
    radius = 4.0 + sqrt (job.duration / 30.0) * 3.0

    -- Color based on status
    color = statusColor job.status

    -- Initial random y position
    pseudoRandom = toNumber ((idx * 17 + 31) `mod` 100) / 100.0 - 0.5
    initY = pseudoRandom * config.height * 0.6
  in
    { id: job.id
    , x: targetX
    , y: initY
    , vx: 0.0
    , vy: 0.0
    , fx: Nullable.null
    , fy: Nullable.null
    , job
    , targetX
    , r: radius
    , color
    }

statusColor :: CompatibilityStatus -> String
statusColor Compatible = "#22c55e"  -- Green
statusColor Failed = "#ef4444"      -- Red
statusColor Untested = "#f59e0b"    -- Amber

-- =============================================================================
-- HATS Rendering
-- =============================================================================

renderSVGContainer :: Config -> Effect Unit
renderSVGContainer config = do
  let
    vbX = -config.width / 2.0
    vbY = -config.height / 2.0
    viewBoxStr = show vbX <> " " <> show vbY <> " " <> show config.width <> " " <> show config.height

    containerTree :: Tree
    containerTree =
      elem SVG
        [ attr "id" "job-beeswarm-svg"
        , attr "viewBox" viewBoxStr
        , attr "width" "100%"
        , attr "height" (show config.height)
        , attr "preserveAspectRatio" "xMidYMid meet"
        , style "display: block;"
        ]
        [ -- Axis line
          elem Line
            [ x1 (-config.width / 2.0 + 50.0)
            , y1 0.0
            , x2 (config.width / 2.0 - 50.0)
            , y2 0.0
            , stroke "#e0e0e0"
            , strokeWidth 1.0
            ]
            []
        , -- Left label (older)
          elem Text
            [ x (-config.width / 2.0 + 55.0)
            , y (config.height / 2.0 - 15.0)
            , fontSize "11"
            , fill "#666"
            , attr "textContent" "← Older jobs"
            ]
            []
        , -- Right label (newer)
          elem Text
            [ x (config.width / 2.0 - 55.0)
            , y (config.height / 2.0 - 15.0)
            , textAnchor "end"
            , fontSize "11"
            , fill "#666"
            , attr "textContent" "Newer jobs →"
            ]
            []
        , -- Jobs container
          elem Group
            [ attr "id" "beeswarm-jobs"
            , class_ "jobs"
            ]
            []
        ]
  _ <- rerender config.containerSelector containerTree
  pure unit

renderJobsHATS :: Config -> Array JobNode -> Effect Unit
renderJobsHATS config nodes = do
  let nodesTree = createJobNodesTree config nodes
  _ <- rerender "#beeswarm-jobs" nodesTree
  pure unit

createJobNodesTree :: Config -> Array JobNode -> Tree
createJobNodesTree config nodes =
  forEach "jobs" Group nodes nodeKey (jobNodeHATS config)
  where
  nodeKey :: JobNode -> String
  nodeKey n = show n.id

-- | Job node with thunked attributes for simulation updates
jobNodeHATS :: Config -> JobNode -> Tree
jobNodeHATS _config node =
  elem Group
    [ thunkedStr "transform" ("translate(" <> show node.x <> "," <> show node.y <> ")")
    , class_ "job-group"
    , thunkedStr "data-id" (show node.id)
    , thunkedStr "data-package" node.job.package
    , thunkedStr "data-compiler" node.job.compiler
    ]
    [ elem Circle
        [ attr "cx" "0"
        , attr "cy" "0"
        , thunkedNum "r" node.r
        , thunkedStr "fill" node.color
        , stroke "#333"
        , strokeWidth 0.5
        , opacity "0.85"
        , attr "cursor" "pointer"
        , class_ "job-circle"
        ]
        []
    ]

-- =============================================================================
-- Force Simulation
-- =============================================================================

startSimulation :: Config -> Array JobNode -> Effect (SimulationHandle JobNodeRow)
startSimulation config nodes = do
  { handle, events } <- runSimulation
    { engine: D3
    , setup: setup "job-beeswarm"
        [ positionX "x" # withX (dynamic _.targetX) # withStrength (static 0.5)
        , positionY "y" # withY (static 0.0) # withStrength (static 0.05)
        , collide "collide" # withRadius (dynamic \n -> n.r + 1.5) # withStrength (static 0.8)
        ]
        # withAlphaDecay 0.01
    , nodes: nodes
    , links: []
    , container: "#beeswarm-jobs"
    , alphaMin: 0.001
    }

  -- Initial render
  initialNodes <- handle.getNodes
  renderJobsHATS config initialNodes

  -- Subscribe to tick events
  _ <- subscribe events \event -> case event of
    Tick _ -> do
      currentNodes <- handle.getNodes
      tickUpdate "#beeswarm-jobs" currentNodes
    Completed -> pure unit
    Started -> pure unit
    Stopped -> pure unit

  pure handle
