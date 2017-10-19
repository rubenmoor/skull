module BotKey.Render
  ( render
  ) where

import EditField.Types as EditField
import Halogen.HTML.Events as Events
import BotKey.Types (Effects, Query(..), Slot, State)
import Data.Function (($))
import Data.Lens ((^.))
import Data.Unit (unit)
import EditField (editField)
import Halogen.Component (ParentHTML)
import Halogen.HTML.Extended (clspan_, faButton_, slot, td, text, tr)
import HttpApp.BotKey.Types (bkLabel, bkSecret)
import Ulff (Ulff)

render
  :: forall eff.
     State
  -> ParentHTML Query EditField.Query Slot (Ulff (Effects eff))
render bk = do
  tr []
    [ td []
        [ slot unit editField (bk ^. bkLabel) (Events.input SetLabel)
        ]
    , td []
        [ clspan_ "monospace cursor-pointer" [ text $ bk ^. bkSecret ]
        ]
    , td []
        [ faButton_ "trash" Delete
        ]
    ]
