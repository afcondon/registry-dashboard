-- | Component.Beeswarm
-- |
-- | Halogen component wrapper for the job timeline beeswarm.
module Component.Beeswarm
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
import Viz.JobBeeswarm as Beeswarm

-- | Component types
type Input = Array MatrixJob

type Output = Void

data Query a = NoQuery a

type Slot = H.Slot Query Output

-- | Internal state
type State =
  { jobs :: Array MatrixJob
  , handle :: Maybe Beeswarm.BeeswarmHandle
  }

-- | Component actions
data Action
  = Initialize
  | Finalize
  | Receive Input

-- | The Beeswarm component
component :: forall m. MonadAff m => H.Component Query Input Output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , finalize = Just Finalize
      , receive = Just <<< Receive
      }
  }

initialState :: Input -> State
initialState jobs =
  { jobs
  , handle: Nothing
  }

render :: forall m. State -> H.ComponentHTML Action () m
render _state =
  HH.div
    [ HP.class_ (HH.ClassName "beeswarm-container") ]
    [ HH.h3
        [ HP.class_ (HH.ClassName "viz-title") ]
        [ HH.text "Job Timeline" ]
    , HH.div
        [ HP.id "beeswarm-svg-container"
        , HP.class_ (HH.ClassName "beeswarm-svg-wrapper")
        ]
        []
    , HH.div
        [ HP.class_ (HH.ClassName "beeswarm-legend") ]
        [ HH.span [ HP.class_ (HH.ClassName "legend-item compatible") ] [ HH.text "Passed" ]
        , HH.span [ HP.class_ (HH.ClassName "legend-item failed") ] [ HH.text "Failed" ]
        , HH.span [ HP.class_ (HH.ClassName "legend-item untested") ] [ HH.text "Pending" ]
        ]
    ]

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    let config = Beeswarm.defaultConfig { containerSelector = "#beeswarm-svg-container" }
    handle <- liftEffect $ Beeswarm.render config state.jobs
    H.modify_ _ { handle = Just handle }

  Finalize -> do
    state <- H.get
    case state.handle of
      Just h -> liftEffect h.stop
      Nothing -> pure unit

  Receive jobs -> do
    H.modify_ _ { jobs = jobs }
    -- Could re-render here if needed
