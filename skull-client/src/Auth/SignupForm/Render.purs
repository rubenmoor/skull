module Auth.SignupForm.Render where

import Halogen.HTML.Events as Events
import Auth.SignupForm.Types (Query(..), State)
import Halogen (ComponentHTML)
import Halogen.HTML (button, div_, h1_, input, label_, p_, text)
import Halogen.HTML.Properties (value)
import Prelude (($))

render :: State -> ComponentHTML Query
render st =
  div_ $
    [ h1_
        [ text "Sign up"
        ]
    , div_
        [ label_
            [ text "Enter username:"
            ]
        ]
    , div_
        [ input
            [ value st.password
            , Events.onValueInput (Events.input SetPassword)
            ]
        ]
    , div_
        [ text st.formError
        ]
    , div_
        [ button
            [ Events.onClick (Events.input_ Submit)
            ]
            [ text "Submit"
            ]
        ]
    ]
