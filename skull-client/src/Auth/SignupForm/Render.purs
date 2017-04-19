module Auth.SignupForm.Render where

import Auth.UserNameField.Types as UserNameField
import Halogen.HTML.Events as Events
import Auth.SignupForm.Types (Query(..), Slot, State, Effects)
import Auth.UserNameField (userNameField)
import Data.Unit (unit)
import Halogen (ParentHTML)
import Halogen.HTML (br_, button, div_, h1_, input, label_, slot, text)
import Halogen.HTML.Properties (ButtonType(..), InputType(..), type_, value)
import Ulff (Ulff)
import Util.HTML (doNothingForm_)

render :: forall eff.
          State
       -> ParentHTML Query UserNameField.Query Slot (Ulff (Effects eff))
render st = doNothingForm_
  [ h1_
      [ text "Sign up"
      ]
  , div_
      [ label_
          [ text "Username:"
          ]
      , br_
      , slot unit userNameField st.userName (Events.input HandleUserNameField)
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
