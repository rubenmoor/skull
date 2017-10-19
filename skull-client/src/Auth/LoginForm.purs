module Auth.LoginForm
  ( loginForm
  ) where

import Halogen.HTML.Events as Events
import Auth.LoginForm.Render (render)
import Auth.LoginForm.Types (Input, Message, Query(..), State, Effects, formError, password, userName, initial)
import Basil (setSessionKey)
import Data.Lens (use, (.=))
import Data.String (null)
import Halogen (Component, ComponentDSL, component)
import Halogen.HTML (HTML)
import HttpApp.User.Api.Types (LoginRequest(..), LoginResponse(..))
import Prelude (type (~>), bind, not, pure, unit, ($), (&&), (<<<), discard)
import Router (Location(..), LoggedInLocation(..), PublicLocation(..), gotoLocation)
import ServerAPI (postUserLogin)
import Ulff (Ulff, mkRequest)

loginForm :: forall eff.
             Component HTML Query Input Message (Ulff (Effects eff))
loginForm =
  component
    { initialState: initial
    , render: render
    , eval: eval
    , receiver: Events.input HandleInput
    }

eval :: forall eff.
        Query ~> ComponentDSL State Query Message (Ulff (Effects eff))
eval = case _ of
    HandleInput str next -> do
      userName .= str
      pure next
    SetUserName str next -> do
      userName .= str
      formError .= ""
      pure next
    SetPassword pwd next -> do
      password .= pwd
      formError .= ""
      pure next
    Submit next -> do
      name <- use userName
      pwd <- use password
      if isValid name && isValid pwd
         then submit name pwd
         else formError .= "Username and passowrd cannot be empty"
      pure next
  where
    submit name pwd = do
      let loginRequest = LoginRequest
            { _lrUserName: name
            , _lrPassword: pwd
            }
      mkRequest (postUserLogin loginRequest) $ case _ of
        LoginFailed msg -> formError .= msg
        LoginSuccess userName sessionKey -> do
          setSessionKey sessionKey
          gotoLocation $ LocLoggedIn $ LocLoggedInPublic LocHome
      pure unit

isValid :: String -> Boolean
isValid = not <<< null
