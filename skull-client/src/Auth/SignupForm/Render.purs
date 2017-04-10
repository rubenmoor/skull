module Auth.SignupForm.Render where

import Auth.UserNameField.Types as UserNameField
import Halogen.HTML.Events as Events
import Auth.SignupForm.Types (Query(..), Slot, State)
import Auth.UserNameField (userNameField)
import Util.HTML (doNothingForm_)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE, log)
import Data.Unit (unit)
import Halogen (ParentHTML)
import Halogen.HTML (AttrName(..), br_, button, div_, form, h1_, input, label_, p_, slot, text)
import Halogen.HTML.Properties (ButtonType(..), InputType(..), attr, type_, value)
import Network.HTTP.Affjax (AJAX)
import Prelude (pure, ($))
import Types (Env)

render :: forall eff.
          Env
       -> State
       -> ParentHTML Query UserNameField.Query Slot (Aff (avar :: AVAR, console :: CONSOLE, ajax :: AJAX | eff))
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
