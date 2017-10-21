module LoggedIn.Types where

import BotKeyList.Types as BotKeyList
import PlayNow.Types as PlayNow
import Home.Types as Home
import Menubar.Types as Menubar
import Navigation.Types as Navigation
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
  { _location :: LoggedInLocation
  , _userName :: UserName
  }

initial :: Input -> State
initial loc =
  { _location: loc
  , _userName: ""
  }

userName :: Lens' State UserName
userName = lens _._userName (\r u -> r { _userName = u })

location :: Lens' State LoggedInLocation
location = lens _._location (\r l -> r { _location = l})

-- query

data Query a
  = Initialize a
  | HandleInput Input a

type ChildQuery =
       Menubar.Query
  <\/> Navigation.Query
  <\/> Home.Query
  <\/> BotKeyList.Query
  <\/> PlayNow.Query
  <\/> Const Void

type ChildSlot =
     Unit
  \/ Unit
  \/ Unit
  \/ Unit
  \/ Unit
  \/ Void

-- output

type Message = Void
