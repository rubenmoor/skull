module Public.Render
  ( render
  ) where

import Halogen (ComponentHTML)
import Halogen.HTML (span_, text)
import Public.Types (Query, State)

render :: State
       -> ComponentHTML Query
render st = span_
  [ text "Home"
  ]
