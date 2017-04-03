module Auth.Signup
  ( signup
  ) where

import Prelude
import Auth.Signup.Types
import Auth.Signup.Render (render)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.State (gets, modify)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Generic.Aeson (decodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (null)
import Halogen (Component, ComponentDSL, component, liftAff)
import Halogen.HTML (HTML)
import Network.HTTP.Affjax (AJAX, AffjaxResponse, post)
import Network.HTTP.StatusCode (StatusCode(..))

signup :: forall eff. Component HTML Query Unit Void (Aff (console :: CONSOLE, ajax :: AJAX | eff))
signup =
  component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }

eval :: forall eff. Query ~> ComponentDSL State Query Void (Aff (console :: CONSOLE , ajax :: AJAX | eff))
eval = case _ of
    SetUsername username next ->
      modify (_
        { username = username
        , userNameLookup = UserNameNothing
        , formError = ""
        }) $> next
    SetPassword password next ->
      modify (_
        { password = password
        , formError = ""
        }) $> next
    CheckUserName next -> do
      username <- gets _.username
      if isValid username
          then lookup username
          else modify (_ { userNameLookup = UserNameInvalid })
      pure next
    Submit next -> do
      username <- gets _.username
      password <- gets _.password
      if isValid username && isValid password
         then submit username password
         else modify (_ { formError = "invalid" })
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
    submit name pwd = do
      let userNewRequest = UserNewRequest
            { unrUserName: name
            , unrPassword: pwd
            }
      (res :: AffjaxResponse Json) <- liftAff $ post "/auth/user/new" (encodeJson userNewRequest)
      let result = case res.status of
            StatusCode 200 -> "Done."
            statusCode     -> show statusCode
      modify (_ { formError = result } )

isValid :: String -> Boolean
isValid = not <<< null
