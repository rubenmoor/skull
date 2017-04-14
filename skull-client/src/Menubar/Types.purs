module Menubar.Types where

import Prelude (Unit, Void, const, unit)

-- input

type Input = Unit

-- state

type State = Unit

initial :: Input -> State
initial = const unit

-- query

data Query a
  = Logout a

-- output

type Message = Void
