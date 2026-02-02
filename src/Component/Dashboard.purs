-- | Component.Dashboard
-- |
-- | Main dashboard component for the Registry Dashboard.
-- | Contains the compiler compatibility matrix and job timeline beeswarm.
module Component.Dashboard
  ( component
  , Query
  ) where

import Prelude

import Component.Beeswarm as Beeswarm
import Component.Matrix as Matrix
import Component.QueueDepth as QueueDepth
import Data.Maybe (Maybe(..))
import Data.Mock (mockMatrix, mockJobs)
import Data.Types (CompatibilityMatrix, MatrixJob)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

-- | Component types
type Input = Unit

type Output = Void

data Query a = NoQuery a

-- | Child slots
type Slots =
  ( matrix :: Matrix.Slot Unit
  , beeswarm :: Beeswarm.Slot Unit
  , queueDepth :: QueueDepth.Slot Unit
  )

_matrix = Proxy :: Proxy "matrix"
_beeswarm = Proxy :: Proxy "beeswarm"
_queueDepth = Proxy :: Proxy "queueDepth"

-- | Internal state
type State =
  { matrix :: CompatibilityMatrix
  , jobs :: Array MatrixJob
  }

-- | Component actions
data Action = Initialize

-- | The Dashboard component
component :: forall m. MonadAff m => H.Component Query Input Output m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }

initialState :: Input -> State
initialState _ =
  { matrix: mockMatrix
  , jobs: mockJobs
  }

-- | Render the dashboard
render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  HH.div
    [ HP.class_ (HH.ClassName "dashboard") ]
    [ renderHeader
    , renderContent state
    , renderFooter
    ]

-- | Render the header
renderHeader :: forall m. H.ComponentHTML Action Slots m
renderHeader =
  HH.header
    [ HP.class_ (HH.ClassName "dashboard-header") ]
    [ HH.h1_ [ HH.text "PureScript Registry Dashboard" ]
    , HH.p
        [ HP.class_ (HH.ClassName "subtitle") ]
        [ HH.text "Matrix Jobs & Compiler Compatibility" ]
    ]

-- | Render the main content area
renderContent :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
renderContent state =
  HH.main
    [ HP.class_ (HH.ClassName "dashboard-content") ]
    [ -- Job timeline beeswarm
      HH.slot _beeswarm unit Beeswarm.component state.jobs absurd
    , -- Queue depth area chart
      HH.slot _queueDepth unit QueueDepth.component state.jobs absurd
    , -- Compatibility matrix
      HH.slot _matrix unit Matrix.component state.matrix absurd
    ]

-- | Render the footer
renderFooter :: forall m. H.ComponentHTML Action Slots m
renderFooter =
  HH.footer
    [ HP.class_ (HH.ClassName "dashboard-footer") ]
    [ HH.p_
        [ HH.text "Built with "
        , HH.a
            [ HP.href "https://github.com/afcondon/hylograph" ]
            [ HH.text "Hylograph" ]
        , HH.text " - Prototype for "
        , HH.a
            [ HP.href "https://github.com/purescript/registry-dev/issues/742" ]
            [ HH.text "registry-dev#742" ]
        ]
    ]

-- | Handle component actions
handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action Slots Output m Unit
handleAction = case _ of
  Initialize -> pure unit
