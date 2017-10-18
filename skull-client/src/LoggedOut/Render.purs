module LoggedOut.Render
  ( render
  ) where

import Auth.LoginForm (loginForm)
import Auth.SignupForm (signupForm)
import Data.Maybe (Maybe(..))
import Halogen (ParentHTML)
import Halogen.Component.ChildPath (cp1, cp2, cp3, cp4)
import Halogen.HTML.Extended (div_, slot')
import LoggedOut.Types (ChildQuery, ChildSlot, Query, State, Effects)
import Menubar (menubar)
import Prelude (absurd, unit)
import Public (public)
import Router (AuthFormLocation(..), LoggedOutLocation(..))
import Ulff (Ulff)

render :: forall eff.
          State
       -> ParentHTML Query ChildQuery ChildSlot (Ulff (Effects eff))
render st =
  div_
    [ slot' cp1 unit menubar Nothing absurd
    , case st.location of
        LocLoggedOutPublic loc -> slot' cp2 unit public loc absurd
        LocAuthForms LocSignupForm -> slot' cp3 unit signupForm "" absurd
        LocAuthForms LocLoginForm  -> slot' cp4 unit loginForm "" absurd
    ]
