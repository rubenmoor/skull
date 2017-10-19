module  Auth.UserNameField
  ( userNameField
  ) where

import Halogen.HTML.Events as Events
import Auth.UserNameField.Render (render)
import Auth.UserNameField.Types (Effects, Input, Message(..), Query(..), State, UserNameCheck(..), userName, userNameLookup, initial)
import Data.Lens (use, (.=))
import Data.String (null)
import Halogen (Component, ComponentDSL, component, raise)
import Halogen.HTML (HTML)
import Prelude (type (~>), bind, not, pure, ($), (<<<), discard)
import ServerAPI (postUserExists)
import HttpApp.User.Api.Types (UserExistsRequest (..))
import Ulff (Ulff, mkRequest', showError)

userNameField :: forall eff.
                 Component HTML Query Input Message (Ulff (Effects eff))
userNameField =
  component
    { initialState: initial
    , render
    , eval
    , receiver: Events.input HandleInput
    }

eval :: forall eff.
        Query ~> ComponentDSL State Query Message (Ulff (Effects eff))
eval = case _ of
    HandleInput str next -> do
      userName .= str
      pure next
    SetUsername str next -> do
      userName .= str
      userNameLookup .= UserNameNothing
      pure next
    CheckUserName next -> do
      name <- use userName
      if isValid name
          then lookup name
          else userNameLookup .= UserNameInvalid
      raise $ UserName name
      pure next
  where
    lookup name = do
      userNameLookup .= UserNameLoading
      let reqBody = UserExistsRequest { _uerName: name }
      mkRequest' showError (postUserExists reqBody) \exists ->
        userNameLookup .= if exists then UserNameExists else UserNameOk

isValid :: String -> Boolean
isValid = not <<< null
