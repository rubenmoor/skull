module Auth.UserNameField.Render where

import Halogen.HTML.Events as Events
import Auth.UserNameField.Types (Query(..), State, UserNameCheck(..))
import DOM.HTML.HTMLInputElement (autofocus)
import Halogen (ComponentHTML)
import Halogen.HTML (span_, text, input)
import Halogen.HTML.Properties (value)
import Prelude (($))
import Util.HTML (clspan_)

render :: State -> ComponentHTML Query
render st = span_
  [ input
     [ value st.userName
     , Events.onValueInput (Events.input SetUsername)
     , Events.onBlur (Events.input_ CheckUserName)
     ]
  , clspan_ "pl1"
      [ text $ case st.userNameLookup of
          UserNameNothing -> ""
          UserNameInvalid -> "Invalid user name"
          UserNameLoading -> "..."
          UserNameExists  -> "Already taken"
          UserNameOk      -> "Good choice!"
      ]
  ]
