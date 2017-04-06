module  Auth.UserNameField where

import Prelude
import Auth.UserNameField.Types
import Halogen.HTML.Events as Events
import Auth.UserNameField.Render (render)
import Control.Monad.Aff (Aff)
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
import Types (ApiSettings)

userNameField :: forall eff.
                 ApiSettings
              -> Component HTML Query Input Message (Aff (console :: CONSOLE, ajax :: AJAX | eff))
userNameField apiSettings =
  component
    { initialState: \name -> initialState { userName = name }
    , render
    , eval: eval apiSettings
    , receiver: Events.input HandleInput
    }

eval :: forall eff.
        ApiSettings
     -> Query ~> ComponentDSL State Query Message (Aff (console :: CONSOLE , ajax :: AJAX | eff))
eval apiSettings = case _ of
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
      pure next
  where
    lookup name = do
      _userNameLookup .= UserNameLoading
      eResult <- runExceptT $ flip runReaderT apiSettings $ postUserExists name
      case eResult of
        Left err    -> liftAff $ log $ errorToString err
        Right true  -> do _userNameLookup .= UserNameExists
                          raise $ UserName name
        Right false -> do _userNameLookup .= UserNameOk
                          raise $ UserName name

isValid :: String -> Boolean
isValid = not <<< null
