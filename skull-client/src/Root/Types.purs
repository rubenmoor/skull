module Root.Types where

import Auth.SignupForm.Types as SignupForm
import Auth.LoginForm.Types as LoginForm
import ErrorMessage.Types as ErrorMessage
import Menubar.Types as Menubar
import Auth.UserNameField.Types (Message(..))
import Basil (STORAGE)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE)
import Data.Const (Const(..))
import Halogen.Component.ChildPath (type (\/), type (<\/>))
import Network.HTTP.Affjax (AJAX)
import Prelude (Unit, Void)

type UserName = String

-- Effects

type Effects e =
  ( avar :: AVAR
  , console :: CONSOLE
  , ajax :: AJAX
  , storage :: STORAGE
  | e
  )

-- Input

type Input = Unit

-- State

data ViewPublic
  = ViewHome

data ViewPrivate
  = ViewBotKeys

data LoggedInRealm
  = LIRealmPublic ViewPublic
  | LIRealmPrivate ViewPrivate

data LoggedOutRealm
  = LORealmPublic ViewPublic
  | LORealmAuthForms ViewAuthForm

data ViewAuthForm
  = ViewSignupForm
  | ViewLoginForm

data State
  = LoggedIn
      { liUserName :: UserName
      , liRealm :: LoggedInRealm
      }
  | LoggedOut LoggedOutRealm

initial :: State
initial = LoggedOut (LORealmAuthForms ViewSignupForm)

-- Query

data Query a
  = Initialize a
  | HandleLogin UserName a
  | HandleGoto a -- todo: routes
  | ShowError ErrorMessage.ErrorMessage a


type ChildQuery =
       ErrorMessage.Query
  <\/> Menubar.Query
  <\/> SignupForm.Query
  <\/> LoginForm.Query
  <\/> Const Void

type ChildSlot =
     Unit
  \/ Unit
  \/ Unit
  \/ Unit
  \/ Void

-- output

type Message = Void
