module  Auth.UserNameField
  ( userNameField
  ) where

import Halogen.HTML.Events as Events
import Auth.UserNameField.Render (render)
import Auth.UserNameField.Types (Effects, Input, Message(..), Query(..), State, UserNameCheck(..), _userName, _userNameLookup, initialState)
import Data.Lens (use, (.=))
import Data.String (null)
import Halogen (Component, ComponentDSL, component, raise)
import Halogen.HTML (HTML)
import Prelude (type (~>), bind, not, pure, ($), (<<<))
import ServerAPI (postUserExists)
import Ulff (Ulff, mkRequest', showError)

userNameField :: forall eff.
                 Component HTML Query Input Message (Ulff (Effects eff))
userNameField =
  component
    { initialState: \name -> initialState { userName = name }
    , render
    , eval
    , receiver: Events.input HandleInput
    }

eval :: forall eff.
        Query ~> ComponentDSL State Query Message (Ulff (Effects eff))
eval = case _ of
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
      mkRequest' showError (postUserExists name) \exists ->
        _userNameLookup .= if exists then UserNameExists else UserNameOk

isValid :: String -> Boolean
isValid = not <<< null
