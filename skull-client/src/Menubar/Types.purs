module Menubar.Types where

import Data.Maybe (Maybe)
import Prelude (Void, id)
import Types (MkRequestEffects)

-- effects

type Effects e = MkRequestEffects e

-- input

type Input = Maybe String -- maybe user name

-- state

type State = Maybe String -- maybe user name

initial :: Input -> State
initial = id

-- query

data Query a
  = HandleInput (Maybe String) a
  | Logout a

-- output

type Message = Void
