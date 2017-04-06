module Auth.SignupForm
  ( signupForm
  ) where

import Prelude
import HttpApp.User.Api.Types
import Auth.UserNameField.Types as UserNameField
import Halogen.HTML.Events as Events
import Auth.SignupForm.Render (render)
import Auth.SignupForm.Types (Input, Query(..), Slot, State(..), initialState)
import Auth.UserNameField (userNameField)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (gets, modify)
import Data.Either (Either(..))
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
    { initialState: \userName -> initialState { userName = userName }
    , render: render apiSettings
    , eval: eval apiSettings
    , receiver: Events.input HandleInput
    }

eval :: forall eff.
        ApiSettings
     -> Query ~> ParentDSL State Query UserNameField.Query Slot Void (Aff (console :: CONSOLE , ajax :: AJAX | eff))
eval apiSettings = case _ of
    HandleInput userName next ->
      modify (_
        { userName = userName
        }) $> next
    HandleUserNameField (UserNameField.UserName userName) next ->
      modify (_
        { userName = userName
        , formError = ""
        }) $> next
    SetPassword password next ->
      modify (_
        { password = password
        , formError = ""
        }) $> next
    Submit next -> do
      userName <- gets _.userName
      password <- gets _.password
      if isValid userName && isValid password
         then submit userName password
         else modify (_ { formError = "invalid" })
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
        Right _     -> modify (_ { formError = "success" } )

isValid :: String -> Boolean
isValid = not <<< null
