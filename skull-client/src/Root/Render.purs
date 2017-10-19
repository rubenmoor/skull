module Root.Render
  ( render
  ) where

import Halogen.Component.ChildPath
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import ErrorMessage (errorMessage)
import Halogen (ParentHTML)
import Halogen.HTML.Extended (div_, slot')
import LoggedIn (loggedIn)
import LoggedOut (loggedOut)
import Prelude (absurd, unit)
import Root.Types (ChildQuery, ChildSlot, Effects, Query, State, location)
import Router (Location(LocLoggedOut, LocLoggedIn))
import Ulff (Ulff)

render :: forall eff.
          State
       -> ParentHTML Query ChildQuery ChildSlot (Ulff (Effects eff))
render st =
  div_
    [ slot' cp1 unit errorMessage Nothing absurd
    , case st ^. location of
        LocLoggedIn loc -> slot' cp2 unit loggedIn loc absurd
        LocLoggedOut loc -> slot' cp3 unit loggedOut loc absurd
    ]
