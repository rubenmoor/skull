module PlayNow.Render
  ( render
  ) where

import BotKey.Types as BotKey
import Halogen.HTML.Events as Events
import Data.Function (($))
import Data.Maybe (Maybe(..))
import Halogen.Component (ParentHTML)
import Halogen.HTML (button, text)
import Halogen.HTML.Extended (cl, cldiv_)
import PlayNow.Types (Effects, Query(..), Slot, State)
import Ulff (Ulff)

render
  :: forall eff.
     State
  -> ParentHTML Query BotKey.Query Slot (Ulff (Effects eff))
render = case _ of
  Nothing -> cldiv_ "bgwhite p1"
    [ button
        [ cl "button"
        , Events.onClick $ Events.input_ (NewGame 4)
        ]
        [ text "New Game"
        ]
    ]
  Just info -> cldiv_ "bgwhite p1"
    [ button
        [ cl "button--pure link-light"
        , Events.onClick $ Events.input_ AbortGame
        ]
        [ text "Abort"
        ]
    ]
