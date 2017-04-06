module Auth.SignupForm.Types where

import Auth.UserNameField.Types as UserNameField
import Data.Unit (Unit)

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

-- Query

data Query a
  = HandleInput String a
  | HandleUserNameField UserNameField.Message a
  | SetPassword String a
  | Submit a

-- Children

type Slot = Unit
