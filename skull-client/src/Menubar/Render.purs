module Menubar.Render
  ( render
  ) where

import Halogen.HTML.Events as Events
import Halogen (ComponentHTML)
import Halogen.HTML (button, text)
import Menubar.Types (Query(..), State)
import Types (Env)
import Util.HTML (cldiv_)

render :: Env
       -> State
       -> ComponentHTML Query
render env st =
  cldiv_ "menubar"
    [ button
        [ Events.onClick (Events.input_ Logout)
        ]
        [ text "log out"
        ]
    ]
