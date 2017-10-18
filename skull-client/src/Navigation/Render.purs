module Navigation.Render
  ( render
  ) where

import Halogen.HTML.Events as Events
import Data.Function (($))
import Halogen.Component (ComponentHTML)
import Navigation.Types (Query(..), State)
import Halogen.HTML.Extended (cl, cldiv_, button, nav, text)

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
