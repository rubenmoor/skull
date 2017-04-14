module  Auth.UserNameField
  ( userNameField
  ) where

import Prelude
import Auth.UserNameField.Types
import Halogen.HTML.Events as Events
import Auth.UserNameField.Render (render)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR, putVar)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (gets, modify)
import Data.Argonaut.Generic.Aeson (decodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Lens (use, (.=))
import Data.String (null)
import Halogen (Component, ComponentDSL, component, liftAff, raise)
import Halogen.HTML (HTML)
import Network.HTTP.Affjax (AJAX, AffjaxResponse, post)
import Network.HTTP.StatusCode (StatusCode(..))
import Servant.PureScript.Affjax (errorToString)
import ServerAPI (postUserExists)
import Types (Env)

userNameField :: forall eff.
                 Env
              -> Component HTML Query Input Message (Aff (avar :: AVAR, console :: CONSOLE, ajax :: AJAX | eff))
userNameField env =
  component
    { initialState: \name -> initialState { userName = name }
    , render
    , eval: eval env
    , receiver: Events.input HandleInput
    }

eval :: forall eff.
        Env
     -> Query ~> ComponentDSL State Query Message (Aff (avar :: AVAR, console :: CONSOLE , ajax :: AJAX | eff))
eval env = case _ of
    HandleInput userName next -> do
      _userName .= userName
      pure next
    SetUsername username next -> do
      _userName .= username
      _userNameLookup .= UserNameNothing
      pure next
    CheckUserName next -> do
      userName <- use _userName
      if isValid userName
          then lookup userName
          else _userNameLookup .= UserNameInvalid
      raise $ UserName userName
      pure next
  where
    lookup name = do
      _userNameLookup .= UserNameLoading
      eResult <- runExceptT $ flip runReaderT env.apiSettings $ postUserExists name
      case eResult of
        Left err    -> do liftAff $ log $ errorToString err
                          liftAff $ putVar env.ajaxError $ { title: "Ajax Error", details: errorToString err}
        Right true  -> _userNameLookup .= UserNameExists
        Right false -> _userNameLookup .= UserNameOk

isValid :: String -> Boolean
isValid = not <<< null
