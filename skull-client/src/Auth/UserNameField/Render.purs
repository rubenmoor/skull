module Auth.UserNameField.Render where

import Halogen.HTML.Events as Events
import Auth.UserNameField.Types (Query(..), State, UserNameCheck(..), userName, userNameLookup)
import Data.Lens ((^.))
import Halogen (ComponentHTML)
import Halogen.HTML.Extended (clspan_, span_, text, input)
import Halogen.HTML.Properties (autofocus, value)
import Prelude (($))

render :: State -> ComponentHTML Query
render st = span_
  [ input
     [ value $ st ^. userName
     , autofocus true
     , Events.onValueInput (Events.input SetUsername)
     , Events.onBlur (Events.input_ CheckUserName)
     ]
  , clspan_ "pl1"
      [ text $ case st ^. userNameLookup of
          UserNameNothing -> ""
          UserNameInvalid -> "Invalid user name"
          UserNameLoading -> "..."
          UserNameExists  -> "Already taken"
          UserNameOk      -> "Good choice!"
      ]
  ]
