module Auth.SignupForm.Render where

import Auth.UserNameField.Types as UserNameField
import Auth.UserNameField (userNameField)
import Halogen.HTML.Events as Events
import Auth.SignupForm.Types (Query(..), Slot, State)
import Data.Unit (unit)
import Halogen (ParentHTML)
import Halogen.HTML (button, div_, h1_, input, label_, p_, text, br_, slot)
import Halogen.HTML.Properties (value)
import Prelude (($))
import Types (ApiSettings)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Network.HTTP.Affjax (AJAX)

render :: forall eff.
          ApiSettings
       -> State
       -> ParentHTML Query UserNameField.Query Slot (Aff (console :: CONSOLE, ajax :: AJAX | eff))
render apiSettings st =
  div_ $
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
