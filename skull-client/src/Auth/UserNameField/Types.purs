module Auth.UserNameField.Types where

import Data.Lens (Lens', lens)
import Types (MkRequestEffects)

-- effects

type Effects e = MkRequestEffects e

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
  { _userNameLookup :: UserNameCheck
  , _userName :: String
  }

initial :: Input -> State
initial str =
  { _userNameLookup: UserNameNothing
  , _userName: str
  }

userNameLookup :: Lens' State UserNameCheck
userNameLookup = lens _._userNameLookup (\r unc -> r { _userNameLookup = unc })

userName :: Lens' State String
userName = lens _._userName (\r unc -> r { _userName = unc })

-- query

data Query a
  = HandleInput String a
  | SetUsername String a
  | CheckUserName a

-- message

data Message = UserName String
