module Root.Types where

import Auth.SignupForm.Types as SignupForm
import ErrorMessage.Types as ErrorMessage
import Data.Const (Const(..))
import Halogen.Component.ChildPath (type (\/), type (<\/>))
import Prelude (Unit, Void)

-- Input

type Input = Unit

-- State

data State
  = ViewSignupForm
  | ViewLoginForm
  | ViewHome

initial :: State
initial = ViewSignupForm

-- Query

data Query a
  = HandleGoto a -- todo: routes
  | ShowError ErrorMessage.ErrorMessage a


type ChildQuery =
       ErrorMessage.Query
  <\/> SignupForm.Query
  <\/> Const Void

type ChildSlot =
     Unit
  \/ Unit
  \/ Void

-- message

type Message = Void
