module Root.Render
  ( render
  ) where

import Types
import Root.Types
import Halogen.Component.ChildPath
import Halogen.HTML.Events as Events
import Auth.SignupForm (signupForm)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE)
import Data.Maybe (Maybe(..))
import ErrorMessage (errorMessage)
import Halogen (ParentHTML)
import Halogen.HTML (button, div_, slot', text)
import Network.HTTP.Affjax (AJAX)
import Prelude (absurd, unit, ($))
import Util.HTML (cldiv_)

render :: forall eff.
          Env
       -> State
       -> ParentHTML Query ChildQuery ChildSlot (Aff (avar :: AVAR, console :: CONSOLE, ajax :: AJAX | eff))
render env st =
  div_
    [ slot' cp1 unit errorMessage Nothing absurd
    , menubar env
    , case st of
        ViewHome -> div_ [] -- todo: home component
        ViewSignupForm -> slot' cp2 unit (signupForm env) "" absurd
        ViewLoginForm -> div_ [] -- todo: login form component
    , button
        [ Events.onClick $ Events.input_ $ ShowError { title: "button", details: "this button has been clicked" }
        ]
        [ text "button" ]
    ]

-- todo: maybe this is a lightweight component
menubar :: forall eff.
           Env
        -> ParentHTML Query ChildQuery ChildSlot (Aff eff)
menubar env = cldiv_ "menubar" []
