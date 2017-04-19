module Root.Render
  ( render
  ) where

import Halogen.Component.ChildPath
import Halogen.HTML.Events as Events
import Auth.LoginForm (loginForm)
import Auth.SignupForm (signupForm)
import Data.Maybe (Maybe(..))
import ErrorMessage (errorMessage)
import Halogen (ParentHTML)
import Halogen.HTML (HTML, div_, slot', text)
import Menubar (menubar)
import Prelude (absurd, unit)
import Root.Types (ChildQuery, ChildSlot, Effects, LoggedInRealm(..), LoggedOutRealm(..), Query(..), State(..), ViewAuthForm(..), ViewPrivate(..), ViewPublic(..))
import Ulff (Ulff)

render :: forall eff.
          State
       -> ParentHTML Query ChildQuery ChildSlot (Ulff (Effects eff))
render st = do
  let mUserName = case st of
        LoggedIn { liUserName: userName } -> Just userName
        LoggedOut _ -> Nothing
  div_
    [ slot' cp1 unit errorMessage Nothing absurd
    , slot' cp2 unit menubar mUserName absurd
    , case st of
        LoggedIn { liRealm: LIRealmPublic ViewHome } -> viewHome
        LoggedIn { liRealm: LIRealmPrivate ViewBotKeys } -> div_ [ text "botkeys" ]
        LoggedOut (LORealmPublic ViewHome) -> viewHome
        LoggedOut (LORealmAuthForms ViewSignupForm) ->
          slot' cp3 unit signupForm "" (Events.input HandleLogin)
        LoggedOut (LORealmAuthForms ViewLoginForm) ->
          slot' cp4 unit loginForm "" (Events.input HandleLogin)
    ]

viewHome :: forall t1 t2. HTML t2 t1
viewHome = div_ [ text "home" ]
