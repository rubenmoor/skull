module LoggedIn.Render
  ( render
  ) where

import BotKeys (botKeys)
import Data.Maybe (Maybe(..))
import Halogen (ParentHTML)
import Halogen.Component.ChildPath (cp1, cp2, cp3)
import Halogen.HTML (div_, slot')
import Home (home)
import LoggedIn.Types (ChildQuery, ChildSlot, Query, State, Effects)
import Menubar (menubar)
import Prelude (absurd, unit)
import Router (LoggedInLocation(..), PrivateLocation(..), PublicLocation(..))
import Ulff (Ulff)

render :: forall eff.
          State
       -> ParentHTML Query ChildQuery ChildSlot (Ulff (Effects eff))
render st =
  div_
    [ slot' cp1 unit menubar (Just st.userName) absurd
    , case st.location of
        LocLoggedInPublic LocHome -> slot' cp2 unit home unit absurd
        LocPrivate LocBotKeys -> slot' cp3 unit botKeys unit absurd
    ]
