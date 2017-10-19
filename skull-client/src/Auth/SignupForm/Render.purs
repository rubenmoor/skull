module Auth.SignupForm.Render where

import Auth.UserNameField.Types as UserNameField
import Halogen.HTML.Events as Events
import Auth.SignupForm.Types (Effects, Query(..), Slot, State, formError, password, userName)
import Auth.UserNameField (userNameField)
import Data.Function (($))
import Data.Lens ((^.))
import Data.Semigroup ((<>))
import Data.String (null)
import Data.Unit (unit)
import Halogen (ParentHTML)
import Halogen.HTML.Extended (cl, cldiv_, doNothingForm_, br_, button, div_, h1_, input, label_, slot, text)
import Halogen.HTML.Properties (ButtonType(..), InputType(..), type_, value)
import Ulff (Ulff)

render :: forall eff.
          State
       -> ParentHTML Query UserNameField.Query Slot (Ulff (Effects eff))
render st = cldiv_ "p1 mx-auto col-4" [ doNothingForm_
  [ h1_
      [ text "Sign up"
      ]
  , cldiv_ "mb1"
      [ label_
          [ text "Username"
          ]
      , br_
      , slot unit userNameField (st ^. userName) (Events.input HandleUserNameField)
      ]
  , cldiv_ "mb1"
      [ label_
          [ text "Password"
          ]
      , br_
      , input
          [ type_ InputPassword
          , value $ st ^. password
          , Events.onValueInput (Events.input SetPassword)
          ]
      ]
  , cldiv_ "mb1 h4" $
      if null $ st ^. formError
         then []
         else
           [ text $ "Form error: " <> st ^. formError
           ]
  , div_
      [ button
          [ cl "button"
          , Events.onClick (Events.input_ Submit)
          , type_ ButtonSubmit
          ]
          [ text "Submit"
          ]
      ]
  ]]
