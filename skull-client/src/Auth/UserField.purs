module  Auth.UserField where

import Prelude
import Auth.UserField.Types
import Auth.UserField.Render (render)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.State (gets, modify)
import Data.Argonaut.Generic.Aeson (decodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.String (null)
import Halogen (Component, ComponentDSL, component, liftAff)
import Halogen.HTML (HTML)
import Halogen.HTML.Events as Events
import Network.HTTP.Affjax (AJAX, AffjaxResponse, post)
import Network.HTTP.StatusCode (StatusCode(..))

userField :: forall eff. Component HTML Query Input Void (Aff (console :: CONSOLE, ajax :: AJAX | eff))
userField =
  component
    { initialState: \name -> initialState { userName = name }
    , render
    , eval
    , receiver: Events.input HandleInput
    }

eval :: forall eff. Query ~> ComponentDSL State Query Void (Aff (console :: CONSOLE , ajax :: AJAX | eff))
eval = case _ of
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
      res <- liftAff $ post "/auth/user/exists" (encodeJson name)
      result <- case decodeJson =<< jsonParser res.response of
            Left  msg   -> do
              liftAff $ log msg
              pure UserNameNothing
            Right true  -> pure UserNameExists
            Right false -> pure UserNameOk
      modify (_ { userNameLookup = result })

isValid :: String -> Boolean
isValid = not <<< null
