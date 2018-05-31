module PlayNow.Render
  ( render
  ) where

import Data.Function (($))
import Data.Maybe (Maybe(..))
import Data.Unit (unit)
import Halogen.Component (ParentHTML)
import Halogen.HTML.Events as Events
import Halogen.HTML.Extended (button, cl, cldiv_, slot, text)
import PlayNow.Types (Effects, Query(..), Slot, State)
import PlayNowGame (playNowGame)
import PlayNowGame.Types as PlayNowGame
import Types (UrlRoot)
import Ulff (Ulff)

render
  :: forall eff.
     UrlRoot
  -> State
  -> ParentHTML Query PlayNowGame.Query Slot (Ulff (Effects eff))
render urlRoot = case _ of
  Nothing -> cldiv_ "bgwhite p1"
    [ button
        [ cl "button"
        , Events.onClick $ Events.input_ (NewGame 4)
        ]
        [ text "New Game"
        ]
    ]
  Just game -> slot unit (playNowGame urlRoot) game (Events.input HandleMsg)
