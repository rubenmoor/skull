module Auth.UserNameField.Render where

import Auth.UserNameField.Types (Query (..), State, UserNameCheck (..))
import Halogen                  (ComponentHTML)
import Halogen.HTML             (span_, text, input)
import Halogen.HTML.Events      as Events
import Halogen.HTML.Properties  (value)
import Prelude                  (($))

render :: State -> ComponentHTML Query
render st = span_
  [ input
     [ value st.userName
     , Events.onValueInput (Events.input SetUsername)
     , Events.onBlur (Events.input_ CheckUserName)
     ]
  , text $ case st.userNameLookup of
      UserNameNothing -> ""
      UserNameInvalid -> "Invalid user name"
      UserNameLoading -> "..."
      UserNameExists  -> "Already taken"
      UserNameOk      -> "Good choice!"
  ]
