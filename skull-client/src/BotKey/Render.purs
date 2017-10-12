module BotKey.Render
  ( render
  ) where

import BotKey.Types (Query, State)
import Data.Function (($))
import Data.String (null)
import Halogen.Component (ComponentHTML)
import Halogen.HTML (li, td, text, tr)
import Util.HTML (clspan_)
import HttpApp.BotKey.Types (BotKey (..), _BotKey)

render
  :: State
  -> ComponentHTML Query
render st = do
  let BotKey bk = st.botKey
  tr []
    [ td []
        [ if null $ bk.bkLabel
             then clspan_ "italic" [ text "unnamed" ]
             else clspan_ "bold"   [ text bk.bkLabel ]
        ]
    , td []
        [ clspan_ "" [ text bk.bkSecret ]
        ]
    ]
