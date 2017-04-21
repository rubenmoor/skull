module LoggedIn.Types where

import BotKeys.Types as BotKeys
import Home.Types as Home
import Menubar.Types as Menubar
import DOM (DOM)
import Data.Const (Const)
import Data.Lens (Lens', lens)
import Halogen.Component.ChildPath (type (\/), type (<\/>))
import Prelude (Unit, Void)
import Router (LoggedInLocation)
import Types (MkRequestEffects)

type UserName = String

-- effects

type Effects e =
  MkRequestEffects
    ( dom :: DOM
    | e
    )

-- input

type Input = LoggedInLocation

-- state

type State =
  { location :: LoggedInLocation
  , userName :: UserName
  }

initial :: Input -> State
initial loc =
  { location: loc
  , userName: ""
  }

_userName :: Lens' State UserName
_userName = lens _.userName (\r u -> r { userName = u })

_location :: Lens' State LoggedInLocation
_location = lens _.location (\r l -> r { location = l})

-- query

data Query a
  = Initialize a

type ChildQuery =
       Menubar.Query
  <\/> Home.Query
  <\/> BotKeys.Query
  <\/> Const Void

type ChildSlot =
     Unit
  \/ Unit
  \/ Unit
  \/ Void

-- output

type Message = Void
