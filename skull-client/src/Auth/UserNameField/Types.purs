module Auth.UserNameField.Types where

import Data.Lens (Lens', lens)

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

_userNameLookup :: Lens' State UserNameCheck
_userNameLookup = lens _.userNameLookup (\r unc -> r { userNameLookup = unc })

_userName :: Lens' State String
_userName = lens _.userName (\r unc -> r { userName = unc })

-- query

data Query a
  = HandleInput String a
  | SetUsername String a
  | CheckUserName a

-- message

data Message = UserName String
