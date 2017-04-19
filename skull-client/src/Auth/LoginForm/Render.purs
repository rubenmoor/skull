module Auth.LoginForm.Render where

import Halogen.HTML.Events as Events
import Auth.LoginForm.Types (Query(..), State)
import Halogen (ComponentHTML)
import Halogen.HTML (br_, button, div_, h1_, input, label_, text)
import Halogen.HTML.Properties (ButtonType(..), InputType(..), type_, value)
import Util.HTML (doNothingForm_)

render :: State
       -> ComponentHTML Query
render st = doNothingForm_
  [ h1_
      [ text "Log in"
      ]
  , div_
      [ label_
          [ text "Username:"
          ]
      , br_
      , input
          [ value st.userName
          , Events.onValueInput (Events.input SetUserName)
          ]
      ]
  , div_
      [ label_
          [ text "Password:"
          ]
      , br_
      , input
          [ type_ InputPassword
          , value st.password
          , Events.onValueInput (Events.input SetPassword)
          ]
      ]
  , div_
      [ text st.formError
      ]
  , div_
      [ button
          [ Events.onClick (Events.input_ Submit)
          , type_ ButtonSubmit
          ]
          [ text "Submit"
          ]
      ]
  ]
