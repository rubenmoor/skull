module Auth.SignupForm
  ( signupForm
  ) where

import Prelude
import Auth.UserNameField.Types as UserNameField
import Halogen.HTML.Events as Events
import Auth.SignupForm.Render (render)
import Auth.SignupForm.Types (Input, Message, Query(..), Slot, State(..), Effects, _formError, _password, _userName, initialState)
import Basil (setSessionKey)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (putVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Data.Either (Either(..))
import Data.Generic (gShow)
import Data.Lens (use, (.=), (.~))
import Data.String (null)
import Halogen (Component, ParentDSL, liftAff, parentComponent, raise)
import Halogen.HTML (HTML)
import HttpApp.User.Api.Types (UserNewRequest(..), UserNewResponse(..))
import Servant.PureScript.Affjax (errorToString)
import ServerAPI (postUserNew)
import Types (Env)
import Ulff (Ulff, mkRequest)

signupForm :: forall eff.
              Component HTML Query Input Message (Ulff (Effects eff))
signupForm =
  parentComponent
    { initialState: \userName -> initialState # _userName .~ userName
    , render: render
    , eval: eval
    , receiver: Events.input HandleInput
    }

eval :: forall eff.
        Query ~> ParentDSL State Query UserNameField.Query Slot Message (Ulff (Effects eff))
eval = case _ of
    HandleInput userName next -> do
      _userName .= userName
      pure next
    HandleUserNameField (UserNameField.UserName userName) next -> do
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
      let userNewRequest = UserNewRequest
            { unrUserName: name
            , unrPassword: pwd
            }
      mkRequest (postUserNew userNewRequest) $ case _ of
        UserNewFailed msg -> _formError .= msg
        UserNewSuccess userName sessionKey -> do
          raise userName -- todo: route to home
          setSessionKey sessionKey

isValid :: String -> Boolean
isValid = not <<< null
