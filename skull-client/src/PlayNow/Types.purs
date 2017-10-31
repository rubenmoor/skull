module PlayNow.Types where

import DOM (DOM)
import Data.Maybe (Maybe(..))
import Data.Unit (Unit)
import Data.Void (Void)
import Game.Types (Card, Game)
import Types (MkRequestEffects)

type Effects e =
  MkRequestEffects
    ( dom :: DOM
    | e
    )

type Input = Unit

type State = Maybe Game

initial :: Input -> State
initial _ = Nothing

data Query a
  = Initialize a
  | NewGame Int a
  | PlayCard Card a
  | AbortGame a

type Slot = Int

type Message = Void
