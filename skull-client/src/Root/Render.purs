module Root.Render
  ( render
  ) where

import Halogen.Component.ChildPath
import Data.Maybe (Maybe(..))
import ErrorMessage (errorMessage)
import Halogen (ParentHTML)
import Halogen.HTML (div_, slot')
import LoggedIn (loggedIn)
import LoggedOut (loggedOut)
import Prelude (absurd, unit)
import Root.Types (ChildQuery, ChildSlot, Effects, Query, State)
import Router (Location(LocLoggedOut, LocLoggedIn))
import Ulff (Ulff)

render :: forall eff.
          State
       -> ParentHTML Query ChildQuery ChildSlot (Ulff (Effects eff))
render st =
  div_
    [ slot' cp1 unit errorMessage Nothing absurd
    , case st.location of
        LocLoggedIn loc -> slot' cp2 unit loggedIn loc absurd
        LocLoggedOut loc -> slot' cp3 unit loggedOut loc absurd
    ]
