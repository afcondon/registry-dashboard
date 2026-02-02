-- | Component.Matrix
-- |
-- | Halogen component wrapper for the compiler compatibility matrix.
-- | Uses HATS (via Viz.CompilerMatrix) for the actual SVG rendering.
module Component.Matrix
  ( component
  , Input
  , Output
  , Query
  , Slot
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Types (CompatibilityMatrix)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Viz.CompilerMatrix as Matrix

-- | Component types
type Input = CompatibilityMatrix

type Output = Void

data Query a = NoQuery a

type Slot = H.Slot Query Output

-- | Internal state
type State =
  { matrix :: CompatibilityMatrix
  , initialized :: Boolean
  }

-- | Component actions
data Action
  = Initialize
  | Receive Input

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
  , initialized: false
  }

-- | Render just a container div - HATS will render the SVG into it
render :: forall m. State -> H.ComponentHTML Action () m
render _state =
  HH.div
    [ HP.class_ (HH.ClassName "matrix-container") ]
    [ -- Container for HATS-rendered SVG
      HH.div
        [ HP.id "matrix-svg-container"
        , HP.class_ (HH.ClassName "matrix-svg-wrapper")
        ]
        []
    , -- Legend
      renderLegend
    ]

-- | Render the legend
renderLegend :: forall m. H.ComponentHTML Action () m
renderLegend =
  HH.div
    [ HP.class_ (HH.ClassName "legend") ]
    [ HH.span [ HP.class_ (HH.ClassName "legend-item compatible") ] [ HH.text "Compatible" ]
    , HH.span [ HP.class_ (HH.ClassName "legend-item untested") ] [ HH.text "Untested" ]
    , HH.span [ HP.class_ (HH.ClassName "legend-item failed") ] [ HH.text "Failed" ]
    ]

-- | Handle component actions
handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    renderMatrix state.matrix
    H.modify_ _ { initialized = true }

  Receive matrix -> do
    H.modify_ _ { matrix = matrix }
    renderMatrix matrix

-- | Render the matrix using HATS
renderMatrix :: forall m. MonadAff m => CompatibilityMatrix -> H.HalogenM State Action () Output m Unit
renderMatrix matrix = do
  let config = Matrix.defaultConfig { containerSelector = "#matrix-svg-container" }
  liftEffect $ Matrix.render config matrix
