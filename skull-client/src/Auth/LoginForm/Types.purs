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
  { _userName :: String
  , _password :: String
  , _formError :: String
  }

initial :: Input -> State
initial str =
  { _userName: str
  , _password: ""
  , _formError: ""
  }

userName :: Lens' State String
userName = lens _._userName (\r str -> r { _userName = str })

formError :: Lens' State String
formError = lens _._formError (\r str -> r { _formError = str })

password :: Lens' State String
password = lens _._password (\r str -> r { _password = str })

-- Query

data Query a
  = HandleInput String a
  | SetUserName String a
  | SetPassword String a
  | Submit a

-- Output

type Message = Void
