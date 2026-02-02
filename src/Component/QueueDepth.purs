-- | Component.QueueDepth
-- |
-- | Halogen component wrapper for the queue depth area chart.
module Component.QueueDepth
  ( component
  , Input
  , Output
  , Query
  , Slot
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Types (MatrixJob)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Viz.QueueDepth as QueueDepth

-- | Component types
type Input = Array MatrixJob

type Output = Void

data Query a = NoQuery a

type Slot = H.Slot Query Output

-- | Internal state
type State =
  { jobs :: Array MatrixJob
  }

-- | Component actions
data Action
  = Initialize
  | Receive Input

-- | The QueueDepth component
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
initialState jobs =
  { jobs
  }

render :: forall m. State -> H.ComponentHTML Action () m
render _state =
  HH.div
    [ HP.class_ (HH.ClassName "queue-depth-container") ]
    [ HH.h3
        [ HP.class_ (HH.ClassName "viz-title") ]
        [ HH.text "Build Queue Depth" ]
    , HH.div
        [ HP.id "queue-depth-svg-container"
        , HP.class_ (HH.ClassName "queue-depth-svg-wrapper")
        ]
        []
    ]

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    let config = QueueDepth.defaultConfig { containerSelector = "#queue-depth-svg-container" }
    liftEffect $ QueueDepth.render config state.jobs

  Receive jobs -> do
    H.modify_ _ { jobs = jobs }
    -- Could re-render here if needed
