module Auth.LoginForm
  ( loginForm
  ) where

import Halogen.HTML.Events as Events
import Auth.LoginForm.Render (render)
import Auth.LoginForm.Types (Input, Message, Query(..), State, Effects, _formError, _password, _userName, initial)
import Basil (setSessionKey)
import Data.Lens (use, (.=))
import Data.String (null)
import Halogen (Component, ComponentDSL, component)
import Halogen.HTML (HTML)
import HttpApp.User.Api.Types (LoginRequest(..), LoginResponse(..))
import Prelude (type (~>), bind, not, pure, unit, ($), (&&), (<<<))
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
    HandleInput userName next -> do
      _userName .= userName
      pure next
    SetUserName userName next -> do
      _userName .= userName
      _formError .= ""
      pure next
    SetPassword password next -> do
      _password .= password
      _formError .= ""
      pure next
    Submit next -> do
      userName <- use _userName
      password <- use _password
      if isValid userName && isValid password
         then submit userName password
         else _formError .= "invalid"
      pure next
  where
    submit name pwd = do
      let loginRequest = LoginRequest
            { lrUserName: name
            , lrPassword: pwd
            }
      mkRequest (postUserLogin loginRequest) $ case _ of
        LoginFailed msg -> _formError .= msg
        LoginSuccess userName sessionKey -> do
          setSessionKey sessionKey
          -- todo: goto location
      pure unit

isValid :: String -> Boolean
isValid = not <<< null
