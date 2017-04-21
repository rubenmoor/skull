module LoggedOut.Types where

import Auth.LoginForm.Types as LoginForm
import Auth.SignupForm.Types as SignupForm
import Menubar.Types as Menubar
import Public.Types as Public
import DOM (DOM)
import Data.Const (Const)
import Data.Lens (Lens', lens)
import Halogen.Component.ChildPath (type (\/), type (<\/>))
import Prelude (Unit, Void)
import Router (LoggedOutLocation)
import Types (MkRequestEffects)

type Effects e =
  MkRequestEffects
    ( dom :: DOM
    | e
    )

type Input = LoggedOutLocation

type State =
  { location :: LoggedOutLocation
  }

initialState :: Input -> State
initialState loc =
  { location: loc
  }

_location :: Lens' State LoggedOutLocation
_location = lens _.location (\r l -> r { location = l})

data Query a
  = HandleInput Input a

type ChildQuery =
       Menubar.Query
  <\/> Public.Query
  <\/> SignupForm.Query
  <\/> LoginForm.Query
  <\/> Const Void

type ChildSlot =
     Unit
  \/ Unit
  \/ Unit
  \/ Unit
  \/ Void

type Message = Void
