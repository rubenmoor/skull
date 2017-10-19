module Auth.SignupForm
  ( signupForm
  ) where

import Prelude
import Auth.UserNameField.Types as UserNameField
import Halogen.HTML.Events as Events
import Auth.SignupForm.Render (render)
import Auth.SignupForm.Types (Input, Message, Query(..), Slot, State, Effects, formError, password, userName, initial)
import Basil (setSessionKey)
import Data.Lens (use, (.=))
import Data.String (null)
import Halogen (Component, ParentDSL, parentComponent)
import Halogen.HTML (HTML)
import HttpApp.User.Api.Types (UserNewRequest(..), UserNewResponse(..))
import Router (Location(..), LoggedInLocation(..), PublicLocation(..), gotoLocation)
import ServerAPI (postUserNew)
import Ulff (Ulff, mkRequest)

signupForm :: forall eff.
              Component HTML Query Input Message (Ulff (Effects eff))
signupForm =
  parentComponent
    { initialState: initial
    , render: render
    , eval: eval
    , receiver: Events.input HandleInput
    }

eval :: forall eff.
        Query ~> ParentDSL State Query UserNameField.Query Slot Message (Ulff (Effects eff))
eval = case _ of
    HandleInput str next -> do
      userName .= str
      pure next
    HandleUserNameField (UserNameField.UserName str) next -> do
      userName .= str
      formError .= ""
      pure next
    SetPassword str next -> do
      password .= str
      formError .= ""
      pure next
    Submit next -> do
      name <- use userName
      pwd  <- use password
      if isValid name && isValid pwd
         then submit name pwd
         else formError .= "Username and password cannot be empty"
      pure next
  where
    submit name pwd = do
      let userNewRequest = UserNewRequest
            { _unrUserName: name
            , _unrPassword: pwd
            }
      mkRequest (postUserNew userNewRequest) $ case _ of
        UserNewFailed msg -> formError .= msg
        UserNewSuccess userName sessionKey -> do
          setSessionKey sessionKey
          gotoLocation $ LocLoggedIn $ LocLoggedInPublic LocHome

isValid :: String -> Boolean
isValid = not <<< null
