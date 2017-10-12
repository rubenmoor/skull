module Navigation.Render
  ( render
  ) where

import Halogen.HTML.Events as Events
import Data.Function (($))
import Halogen.Component (ComponentHTML)
import Halogen.HTML (button, nav, text)
import Navigation.Types (Query(..), State)
import Util.HTML (cl, cldiv_)

render
  :: State
  -> ComponentHTML Query
render _ = nav
  [ cl "clearfix bgmedium"
  ]
  [ cldiv_ "sm-col"
      [ button
          [ cl "button--navigation"
          , Events.onClick $ Events.input_ GotoHome
          ]
          [ text "Home"
          ]
      , button
          [ cl "button--navigation"
          , Events.onClick $ Events.input_ GotoBotKeys
          ]
          [ text "BotKeys"
          ]
      ]
  ]
