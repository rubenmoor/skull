module BotKeys.Render
  ( render
  ) where

import BotKeys.Types (Query, State)
import Halogen (ComponentHTML)
import Halogen.HTML (text)
import Util.HTML (cldiv_)

render :: State
       -> ComponentHTML Query
render st = cldiv_ "p1"
  [ text "BotKeys"
  ]
