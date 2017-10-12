module Navigation.Types where

import DOM (DOM)
import Data.Unit (Unit, unit)
import Data.Void (Void)
import Types (MkRequestEffects)

type Effects e =
  MkRequestEffects
    ( dom :: DOM
    | e
    )

type Input = Unit

type State = Unit

initial :: Input -> State
initial _ = unit

data Query a
  = HandleInput Input a
  | GotoHome a
  | GotoBotKeys a

type Message = Void
