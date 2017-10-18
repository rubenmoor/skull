module LoggedIn.Render
  ( render
  ) where

import BotKeyList (botKeyList)
import Data.Maybe (Maybe(..))
import Halogen (ParentHTML)
import Halogen.Component.ChildPath (cp1, cp2, cp3, cp4)
import Halogen.HTML.Extended (div_, slot')
import Home (home)
import LoggedIn.Types (ChildQuery, ChildSlot, Query, State, Effects)
import Menubar (menubar)
import Navigation (navigation)
import Prelude (absurd, unit)
import Router (LoggedInLocation(..), PrivateLocation(..), PublicLocation(..))
import Ulff (Ulff)

render :: forall eff.
          State
       -> ParentHTML Query ChildQuery ChildSlot (Ulff (Effects eff))
render st =
  div_
    [ slot' cp1 unit menubar (Just st.userName) absurd
    , slot' cp2 unit navigation unit absurd
    , case st.location of
        LocLoggedInPublic LocHome -> slot' cp3 unit home unit absurd
        LocPrivate LocBotKeys -> slot' cp4 unit botKeyList unit absurd
    ]
