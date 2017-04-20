module Root.Types where

import LoggedIn.Types as LoggedIn
import LoggedOut.Types as LoggedOut
import ErrorMessage.Types as ErrorMessage
import Basil (STORAGE)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE)
import Data.Const (Const)
import Data.Lens (Lens', lens)
import Halogen.Component.ChildPath (type (\/), type (<\/>))
import Network.HTTP.Affjax (AJAX)
import Prelude (Unit, Void)
import Router (Location(..), LoggedInLocation(..), PublicLocation(..))
import Types (Error)

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

type State =
  { location :: Location
  }

initial :: Input -> State
initial _ =
  { location: LocLoggedIn (LocLoggedInPublic LocHome)
  }

_location :: Lens' State Location
_location = lens _.location (\r l -> r { location = l})

-- Query

data Query a
  -- | HandleGoto a -- todo: routes
  = ShowError Error a


type ChildQuery =
       ErrorMessage.Query
  <\/> LoggedIn.Query
  <\/> LoggedOut.Query
  <\/> Const Void

type ChildSlot =
     Unit
  \/ Unit
  \/ Unit
  \/ Void

-- output

type Message = Void
