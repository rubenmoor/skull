module Auth.SignupForm.Types where

import Auth.UserNameField.Types as UserNameField
import Data.Lens (Lens', lens)
import Data.Unit (Unit)
import Prelude (Void)

-- Input

type Input = String

-- State

type State =
  { userName :: String
  , password :: String
  , formError :: String
  }

initialState :: State
initialState =
  { userName: ""
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
  | HandleUserNameField UserNameField.Message a
  | SetPassword String a
  | Submit a

-- Children

type Slot = Unit

-- Output

type Message = Void
