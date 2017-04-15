module Root.Render
  ( render
  ) where

import Types
import Root.Types
import Halogen.Component.ChildPath
import Halogen.HTML.Events as Events
import Auth.LoginForm (loginForm)
import Auth.SignupForm (signupForm)
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))
import ErrorMessage (errorMessage)
import Halogen (ParentHTML)
import Halogen.HTML (HTML(..), button, div_, slot', text)
import Menubar (menubar)
import Prelude (absurd, unit, ($))
import Util.HTML (cldiv_)

render :: forall eff.
          Env
       -> State
       -> ParentHTML Query ChildQuery ChildSlot (Aff (Effects eff))
render env st = do
  let mUserName = case st of
        LoggedIn { liUserName: userName } -> Just userName
        LoggedOut _ -> Nothing
  div_
    [ slot' cp1 unit errorMessage Nothing absurd
    , slot' cp2 unit (menubar env) mUserName absurd
    , case st of
        LoggedIn { liRealm: LIRealmPublic ViewHome } -> viewHome
        LoggedIn { liRealm: LIRealmPrivate ViewBotKeys } -> div_ [ text "botkeys" ]
        LoggedOut (LORealmPublic ViewHome) -> viewHome
        LoggedOut (LORealmAuthForms ViewSignupForm) ->
          slot' cp3 unit (signupForm env) "" (Events.input HandleLogin)
        LoggedOut (LORealmAuthForms ViewLoginForm) ->
          slot' cp4 unit (loginForm env) "" (Events.input HandleLogin)
    , button
        [ Events.onClick $ Events.input_ $ ShowError { title: "button", details: "this button has been clicked." }
        ]
        [ text "button"
        ]
    ]

viewHome :: forall t1 t2. HTML t2 t1
viewHome = div_ [ text "home" ]
