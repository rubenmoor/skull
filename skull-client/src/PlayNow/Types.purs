module PlayNow.Types where

import DOM (DOM)
import Data.Void (Void)
import Data.Maybe (Maybe(..))
import Data.Unit (Unit)
import Types (MkRequestEffects)

import PlayNowGame.Types as PlayNowGame

type Effects e =
  MkRequestEffects
    ( dom :: DOM
    | e
    )

type Input = Unit

type State = Maybe PlayNowGame.State

initial :: Input -> State
initial _ = Nothing

data Query a
  = Initialize a
  | NewGame Int a
  | HandleMsg PlayNowGame.Message a

type Slot = Unit

type Message = Void
