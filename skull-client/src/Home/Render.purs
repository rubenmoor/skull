module Home.Render
  ( render
  ) where

import Halogen (ComponentHTML)
import Halogen.HTML (text)
import Home.Types (Query, State)
import Util.HTML (cldiv_)

render :: State
       -> ComponentHTML Query
render st = cldiv_ "p1"
  [ text "Home"
  ]
