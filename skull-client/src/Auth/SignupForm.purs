module Auth.SignupForm
  ( signupForm
  ) where

import Prelude
import Auth.SignupForm.Types
import HttpApp.User.Api.Types
import Auth.SignupForm.Render (render)
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

signupForm :: forall eff. Component HTML Query Input Void (Aff (console :: CONSOLE, ajax :: AJAX | eff))
signupForm =
  component
    { initialState: \userName -> initialState { userName = userName }
    , render
    , eval
    , receiver: const Nothing
    }

eval :: forall eff. Query ~> ComponentDSL State Query Void (Aff (console :: CONSOLE , ajax :: AJAX | eff))
eval = case _ of
    SetUsername userName next ->
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
      (res :: AffjaxResponse Json) <- liftAff $ post "/auth/user/new" (encodeJson userNewRequest)
      let result = case res.status of
            StatusCode 200 -> "Done."
            statusCode     -> show statusCode
      modify (_ { formError = result } )

isValid :: String -> Boolean
isValid = not <<< null
