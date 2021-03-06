module MainFrame.View where

import Prelude hiding (div)
import Css (classNames)
import Dashboard.View (renderDashboardState)
import Data.Either (Either(..))
import Data.Lens (view)
import Effect.Aff.Class (class MonadAff)
import Halogen (ComponentHTML)
import Halogen.Extra (renderSubmodule)
import Halogen.HTML (div)
import MainFrame.Lenses (_currentSlot, _dashboardState, _subState, _toast, _welcomeState)
import MainFrame.Types (Action(..), ChildSlots, State)
import Toast.View (renderToast)
import Welcome.View (renderWelcomeState)

render :: forall m. MonadAff m => State -> ComponentHTML Action ChildSlots m
render state =
  let
    currentSlot = view _currentSlot state
  in
    div [ classNames [ "h-full" ] ]
      [ case view _subState state of
          Left _ -> renderSubmodule _welcomeState WelcomeAction renderWelcomeState state
          Right _ -> renderSubmodule _dashboardState DashboardAction (renderDashboardState currentSlot) state
      , renderSubmodule _toast ToastAction renderToast state
      ]
