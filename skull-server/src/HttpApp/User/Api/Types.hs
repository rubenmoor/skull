{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module HttpApp.User.Api.Types where

import           Data.Aeson         (FromJSON, ToJSON)
import           Data.Text          (Text)
import           GHC.Generics       (Generic)

import           HttpApp.User.Types (UserName)

data UserNewRequest = UserNewRequest
  { _unrUserName :: UserName
  , _unrPassword :: Text
  } deriving (Generic, FromJSON, ToJSON)

data UserNewResponse
  = UserNewSuccess UserName Text
  | UserNewFailed Text
  deriving (Generic, ToJSON)

data UserNameResponse = UserNameResponse
  { _unrName :: UserName
  } deriving (Generic, ToJSON)

data LoginRequest = LoginRequest
  { _lrUserName :: UserName
  , _lrPassword :: Text
  } deriving (Generic, FromJSON, ToJSON)

data LoginResponse
  = LoginSuccess UserName Text
  | LoginFailed Text
  deriving (Generic, ToJSON)

data LogoutResponse = LogoutResponse
  deriving (Generic, ToJSON)

data UserExistsRequest = UserExistsRequest
  { _uerName :: UserName
  } deriving (Generic, FromJSON, ToJSON)
