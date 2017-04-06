module  Auth.UserNameField where

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Types (ApiSettings)
import Prelude
import Auth.UserNameField.Types
import Auth.UserNameField.Render (render)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.State (gets, modify)
import Data.Argonaut.Generic.Aeson (decodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.String (null)
import Halogen (Component, ComponentDSL, component, liftAff, raise)
import Halogen.HTML (HTML)
import Halogen.HTML.Events as Events
import Network.HTTP.Affjax (AJAX, AffjaxResponse, post)
import Network.HTTP.StatusCode (StatusCode(..))
import ServerAPI (postUserExists)
import Servant.PureScript.Affjax (errorToString)

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
    HandleInput userName next ->
      modify (_
        { userName = userName
        }) $> next
    SetUsername username next ->
      modify (_
        { userName = username
        , userNameLookup = UserNameNothing
        }) $> next
    CheckUserName next -> do
      userName <- gets _.userName
      if isValid userName
          then lookup userName
          else modify (_ { userNameLookup = UserNameInvalid })
      pure next
  where
    lookup name = do
      modify (_ { userNameLookup = UserNameLoading })
      eResult <- runExceptT $ flip runReaderT apiSettings $ postUserExists name
      case eResult of
        Left err    -> liftAff $ log $ errorToString err
        Right true  -> modify (_ { userNameLookup = UserNameExists })
        Right false -> do modify (_ { userNameLookup = UserNameOk })
                          raise $ ValidUserName name

isValid :: String -> Boolean
isValid = not <<< null
