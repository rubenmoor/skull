module Public.Render
  ( render
  ) where

import Halogen (ComponentHTML)
import Public.Types (Query, State)
import Halogen.HTML.Extended (cldiv_, text)

render :: State
       -> ComponentHTML Query
render st = cldiv_ "p1"
  [ text "Public"
  ]
