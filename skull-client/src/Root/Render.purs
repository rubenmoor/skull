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
import Types (UrlRoot)
import Ulff (Ulff)

render
  :: forall eff.
     UrlRoot
  -> State
  -> ParentHTML Query ChildQuery ChildSlot (Ulff (Effects eff))
render urlRoot st =
  div_
    [ slot' cp1 unit errorMessage Nothing absurd
    , case st ^. location of
        LocLoggedIn loc -> slot' cp2 unit (loggedIn urlRoot) loc absurd
        LocLoggedOut loc -> slot' cp3 unit loggedOut loc absurd
    ]
