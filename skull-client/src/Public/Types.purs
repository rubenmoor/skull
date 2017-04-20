module Public.Types where

import Data.Lens (Lens', lens)
import Prelude (Unit, Void)
import Router (PublicLocation)

type Effects e = e

type Input = PublicLocation

type State =
  { location :: PublicLocation
  }

initialState :: Input -> State
initialState loc =
  { location: loc
  }

_location :: Lens' State PublicLocation
_location = lens _.location (\r l -> r { location = l})

data Query a
  = HandleInput Input a

type Message = Void
