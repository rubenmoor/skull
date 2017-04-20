module Home.Types where

import Prelude (Unit, Void)

type Effects e = e

type Input = Unit

type State =
  {
  }

initialState :: Input -> State
initialState _ =
  {
  }

data Query a
  = HandleInput Input a

type Message = Void
