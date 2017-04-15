module Menubar.Types where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE)
import Data.Maybe (Maybe)
import Network.HTTP.Affjax (AJAX)
import Prelude (Unit, Void, const, id, unit)

-- effects

type Effects e =
  ( avar :: AVAR
  , console :: CONSOLE
  , ajax :: AJAX
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

-- output

type Message = Void
