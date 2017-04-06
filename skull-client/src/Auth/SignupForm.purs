module Auth.SignupForm
  ( signupForm
  ) where

import Prelude
import HttpApp.User.Api.Types
import Auth.UserNameField.Types as UserNameField
import Halogen.HTML.Events as Events
import Auth.SignupForm.Render (render)
import Auth.SignupForm.Types (Input, Query(..), Slot, State(..), _formError, _password, _userName, initialState)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (gets, modify)
import Data.Either (Either(..))
import Data.Lens (use, view, (.=), (.~))
import Data.String (null)
import Halogen (Component, ParentDSL, parentComponent, liftAff)
import Halogen.HTML (HTML)
import Network.HTTP.Affjax (AJAX)
import Servant.PureScript.Affjax (errorToString)
import ServerAPI (postUserNew)
import Types (ApiSettings)

signupForm :: forall eff.
              ApiSettings
           -> Component HTML Query Input Void (Aff (console :: CONSOLE, ajax :: AJAX | eff))
signupForm apiSettings =
  parentComponent
    -- { initialState: \userName -> initialState { userName = userName }
    { initialState: \userName -> initialState # _userName .~ userName
    , render: render apiSettings
    , eval: eval apiSettings
    , receiver: Events.input HandleInput
    }

eval :: forall eff.
        ApiSettings
     -> Query ~> ParentDSL State Query UserNameField.Query Slot Void (Aff (console :: CONSOLE , ajax :: AJAX | eff))
eval apiSettings = case _ of
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
      eResult <- runExceptT $ flip runReaderT apiSettings $ postUserNew userNewRequest
      case eResult of
        Left err    -> liftAff $ log $ errorToString err
        Right _     -> _formError .= "success"

isValid :: String -> Boolean
isValid = not <<< null
