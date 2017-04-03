module Auth.Signup.Render where

import Halogen.HTML.Events as Events
import Auth.Signup.Types (Query(..), State, UserNameCheck(..))
import Halogen (ComponentHTML)
import Halogen.HTML (button, div_, h1_, input, label_, p_, text)
import Halogen.HTML.Properties (value)
import Prelude (($))

render :: State -> ComponentHTML Query
render st =
  div_ $
    [ h1_ [ text "Sign up" ]
    , div_
        [ label_ [ text "Enter username:" ]
        , div_ [
            input
              [ value st.username
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
    , div_ [
        input
          [ value st.password
          , Events.onValueInput (Events.input SetPassword)
          ]
        ]
    , div_ [
          text st.formError
        ]
    , div_ [
        button
          [ Events.onClick (Events.input_ Submit)
          ]
          [ text "Submit"
          ]
        ]
    ]
