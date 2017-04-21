module Auth.LoginForm.Types where

import DOM (DOM)
import Data.Lens (Lens', lens)
import Prelude (Void)
import Types (MkRequestEffects)

-- Effects

type Effects e =
  MkRequestEffects
    ( dom :: DOM
    | e
    )

-- Input

type Input = String

-- State

type State =
  { userName :: String
  , password :: String
  , formError :: String
  }

initial :: Input -> State
initial str =
  { userName: str
  , password: ""
  , formError: ""
  }

_userName :: Lens' State String
_userName = lens _.userName (\r str -> r { userName = str })

_formError :: Lens' State String
_formError = lens _.formError (\r str -> r { formError = str })

_password :: Lens' State String
_password = lens _.password (\r str -> r { password = str })

-- Query

data Query a
  = HandleInput String a
  | SetUserName String a
  | SetPassword String a
  | Submit a

-- Output

type Message = Void
