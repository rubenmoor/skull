module PlayNowGame.Types where

import DOM (DOM)
import Game.Types (Card, Game)
import Types (MkRequestEffects)
import Data.Lens (lens, Lens')

type Effects e =
  MkRequestEffects
    ( dom :: DOM
    | e
    )

type Input = State

type State =
  { _game :: Game
  , _humanPlayerKey :: String
  }

game :: Lens' State Game
game = lens _._game (\r g -> r { _game = g })

humanPlayerKey :: Lens' State String
humanPlayerKey = lens _._humanPlayerKey (\r h -> r { _humanPlayerKey = h})

initial :: Input -> State
initial st = st

data Query a
  = HandleInput Input a
  | PlayCard Card a
  | AbortGame a

data Message = Delete
