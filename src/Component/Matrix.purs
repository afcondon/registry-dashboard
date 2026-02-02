-- | Component.Matrix
-- |
-- | Halogen component wrapper for the compiler compatibility matrix.
-- | Manages expand/collapse state for version groups.
module Component.Matrix
  ( component
  , Input
  , Output
  , Query
  , Slot
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Types (CompatibilityMatrix, MajorVersion)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Viz.CompilerMatrix as Matrix

-- | Component types
type Input = CompatibilityMatrix

type Output = Void

data Query a = NoQuery a

type Slot = H.Slot Query Output

-- | Internal state
type State =
  { matrix :: CompatibilityMatrix
  , expanded :: Set MajorVersion
  , initialized :: Boolean
  }

-- | Component actions
data Action
  = Initialize
  | Receive Input
  | ToggleGroup MajorVersion

-- | The Matrix component
component :: forall m. MonadAff m => H.Component Query Input Output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , receive = Just <<< Receive
      }
  }

initialState :: Input -> State
initialState matrix =
  { matrix
  , expanded: Set.empty
  , initialized: false
  }

render :: forall m. State -> H.ComponentHTML Action () m
render _state =
  HH.div
    [ HP.class_ (HH.ClassName "matrix-container") ]
    [ HH.div
        [ HP.id "matrix-svg-container"
        , HP.class_ (HH.ClassName "matrix-svg-wrapper")
        ]
        []
    , renderLegend
    ]

renderLegend :: forall m. H.ComponentHTML Action () m
renderLegend =
  HH.div
    [ HP.class_ (HH.ClassName "legend") ]
    [ HH.span [ HP.class_ (HH.ClassName "legend-item compatible") ] [ HH.text "Compatible" ]
    , HH.span [ HP.class_ (HH.ClassName "legend-item untested") ] [ HH.text "Untested" ]
    , HH.span [ HP.class_ (HH.ClassName "legend-item failed") ] [ HH.text "Failed" ]
    , HH.span [ HP.class_ (HH.ClassName "legend-hint") ] [ HH.text "Click version headers to expand/collapse" ]
    ]

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    let config = Matrix.defaultConfig { containerSelector = "#matrix-svg-container" }

    -- Create emitter for group toggle events
    { emitter, listener } <- liftEffect HS.create

    -- Subscribe to toggle events
    _ <- H.subscribe emitter

    -- Set up click handlers that notify the emitter
    liftEffect $ Matrix.setupClickHandlers config \major ->
      HS.notify listener (ToggleGroup major)

    -- Initial render
    liftEffect $ Matrix.render config state.matrix state.expanded
    H.modify_ _ { initialized = true }

  Receive matrix -> do
    state <- H.get
    H.modify_ _ { matrix = matrix }
    let config = Matrix.defaultConfig { containerSelector = "#matrix-svg-container" }
    liftEffect $ Matrix.render config matrix state.expanded

  ToggleGroup major -> do
    state <- H.get
    let newExpanded = if Set.member major state.expanded
                      then Set.delete major state.expanded
                      else Set.insert major state.expanded
    H.modify_ _ { expanded = newExpanded }
    let config = Matrix.defaultConfig { containerSelector = "#matrix-svg-container" }
    liftEffect $ Matrix.render config state.matrix newExpanded
