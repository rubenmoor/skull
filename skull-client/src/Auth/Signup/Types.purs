module Auth.Signup.Types where

import Data.Generic (class Generic)

data UserNameCheck
  = UserNameNothing
  | UserNameLoading
  | UserNameExists
  | UserNameOk
  | UserNameInvalid

type State =
  { userNameLookup :: UserNameCheck
  , username :: String
  , password :: String
  , formError :: String
  }

initialState :: State
initialState =
  { userNameLookup: UserNameNothing
  , username: ""
  , password: ""
  , formError: ""
  }

data Query a
  = SetUsername String a
  | SetPassword String a
  | CheckUserName a
  | Submit a

-- api

data UserNewRequest = UserNewRequest
  { unrUserName :: String
  , unrPassword :: String
  }

derive instance genericUserNewRequest :: Generic UserNewRequest
