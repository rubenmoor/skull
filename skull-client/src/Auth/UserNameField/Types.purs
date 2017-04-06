module Auth.UserNameField.Types where

-- input

type Input = String

-- state

data UserNameCheck
  = UserNameNothing
  | UserNameLoading
  | UserNameExists
  | UserNameOk
  | UserNameInvalid

type State =
  { userNameLookup :: UserNameCheck
  , userName :: String
  }

initialState :: State
initialState =
  { userNameLookup: UserNameNothing
  , userName: ""
  }

-- query

data Query a
  = HandleInput String a
  | SetUsername String a
  | CheckUserName a

-- message

data Message = ValidUserName String
