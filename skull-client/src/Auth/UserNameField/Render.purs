module Auth.UserNameField.Render where

import Auth.UserNameField.Types (Query (..), State, UserNameCheck (..))
import Halogen                  (ComponentHTML)
import Halogen.HTML             (button, div_, h1_, input, label_, p_,
                                 text)
import Halogen.HTML.Events      as Events
import Halogen.HTML.Properties  (value)
import Prelude                  (($))

render :: State -> ComponentHTML Query
render st = div_ $
  [ div_
      [ label_
          [ text "Enter username:"
          ]
      , div_
          [ input
              [ value st.userName
              , Events.onValueInput (Events.input SetUsername)
              , Events.onBlur (Events.input_ CheckUserName)
              ]
          ]
      , p_
          [ text $ case st.userNameLookup of
               UserNameNothing -> ""
               UserNameInvalid -> "Invalid user name"
               UserNameLoading -> "..."
               UserNameExists  -> "Already taken"
               UserNameOk      -> "Good choice!"
          ]
      ]
  ]
