module Auth.SignupForm.Render where

import Auth.UserNameField.Types as UserNameField
import Halogen.HTML.Events as Events
import Auth.SignupForm.Types (Query(..), Slot, State, Effects)
import Auth.UserNameField (userNameField)
import Control.Monad.Aff (Aff)
import Data.Unit (unit)
import Halogen (ParentHTML)
import Halogen.HTML (AttrName(..), br_, button, div_, form, h1_, input, label_, p_, slot, text)
import Halogen.HTML.Properties (ButtonType(..), InputType(..), attr, type_, value)
import Types (Env)
import Util.HTML (doNothingForm_)

render :: forall eff.
          Env
       -> State
       -> ParentHTML Query UserNameField.Query Slot (Aff (Effects eff))
render env st = doNothingForm_
  [ h1_
      [ text "Sign up"
      ]
  , div_
      [ label_
          [ text "Username:"
          ]
      , br_
      , slot unit (userNameField env) st.userName (Events.input HandleUserNameField)
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
