module Auth.SignupForm.Types where

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
  = SetUsername String a
  | SetPassword String a
  | Submit a
