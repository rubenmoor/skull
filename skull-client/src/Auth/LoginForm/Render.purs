module Auth.LoginForm.Render where

import Halogen.HTML.Events as Events
import Auth.LoginForm.Types (Query(..), State, formError, password, userName)
import Data.Function (($))
import Data.Lens ((^.))
import Data.Semigroup ((<>))
import Data.String (null)
import Halogen (ComponentHTML)
import Halogen.HTML.Extended (cl, cldiv_, doNothingForm_, br_, button, div_, h1_, input, label_, text)
import Halogen.HTML.Properties (ButtonType(..), InputType(..), autofocus, type_, value)

render :: State
       -> ComponentHTML Query
render st = cldiv_ "p1 mx-auto col-4" [ doNothingForm_
  [ h1_
      [ text "Log in"
      ]
  , cldiv_ "mb1"
      [ label_
          [ text "Username"
          ]
      , br_
      , input
          [ value $ st ^. userName
          , autofocus true
          , Events.onValueInput (Events.input SetUserName)
          ]
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
