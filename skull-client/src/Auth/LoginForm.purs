module Auth.LoginForm
  ( loginForm
  ) where

import Halogen.HTML.Events as Events
import Auth.LoginForm.Render (render)
import Auth.LoginForm.Types (Input, Message, Query(..), State, Effects, _formError, _password, _userName, initial)
import Basil (setSessionKey)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (putVar)
import Control.Monad.Aff.Console (log)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Data.Either (Either(..))
import Data.Lens (use, (.=))
import Data.String (null)
import Halogen (Component, ComponentDSL, component, liftAff, raise)
import Halogen.HTML (HTML)
import HttpApp.User.Api.Types (LoginRequest(..), LoginResponse(..))
import Prelude (type (~>), bind, flip, not, pure, ($), (&&), (<<<))
import Servant.PureScript.Affjax (errorToString)
import ServerAPI (postUserLogin)
import Types (Env)

loginForm :: forall eff.
             Env
          -> Component HTML Query Input Message (Aff (Effects eff))
loginForm env =
  component
    { initialState: initial
    , render: render env
    , eval: eval env
    , receiver: Events.input HandleInput
    }

eval :: forall eff.
        Env
     -> Query ~> ComponentDSL State Query Message (Aff (Effects eff))
eval env = case _ of
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
      eResult <- runExceptT $ flip runReaderT env.apiSettings $ postUserLogin loginRequest
      case eResult of
        Left err    -> liftAff do log $ errorToString err
                                  putVar env.ajaxError { title: "AJAX error", details: errorToString err }
        Right (LoginFailed msg) -> _formError .= msg
        Right (LoginSuccess userName sessionKey) -> do
          raise userName
          setSessionKey sessionKey

isValid :: String -> Boolean
isValid = not <<< null
