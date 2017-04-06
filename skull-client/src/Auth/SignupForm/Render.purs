module Auth.SignupForm.Render where

import Auth.UserNameField.Types as UserNameField
import Halogen.HTML.Events as Events
import Auth.SignupForm.Types (Query(..), Slot, State)
import Auth.UserNameField (userNameField)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Data.Unit (unit)
import Halogen (ParentHTML)
import Halogen.HTML (AttrName(..), br_, button, div_, form, h1_, input, label_, p_, slot, text)
import Halogen.HTML.Properties (ButtonType(..), InputType(..), attr, type_, value)
import Network.HTTP.Affjax (AJAX)
import Prelude (pure, ($))
import Types (ApiSettings)
import Common.DoNothingForm (doNothingForm_)

render :: forall eff.
          ApiSettings
       -> State
       -> ParentHTML Query UserNameField.Query Slot (Aff (console :: CONSOLE, ajax :: AJAX | eff))
render apiSettings st = doNothingForm_
  [ h1_
      [ text "Sign up"
      ]
  , div_
      [ label_
          [ text "Username:"
          ]
      , br_
      , slot unit (userNameField apiSettings) st.userName (Events.input HandleUserNameField)
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
