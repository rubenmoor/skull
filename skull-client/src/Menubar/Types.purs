module Menubar.Types where

import DOM (DOM)
import Data.Maybe (Maybe)
import Prelude (Void, id)
import Types (MkRequestEffects)

-- effects

type Effects e =
  MkRequestEffects
    ( dom :: DOM
    | e
    )

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
  -- todo: save current location and return after successful login
  | GotoSignupForm a
  | GotoLoginForm a

-- output

type Message = Void
