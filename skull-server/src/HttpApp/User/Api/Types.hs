{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module HttpApp.User.Api.Types where

import           Data.Aeson         (FromJSON, ToJSON)
import           Data.Text          (Text)
import           GHC.Generics       (Generic)

import           HttpApp.User.Types (UserName)

data UserNewRequest = UserNewRequest
  { unrUserName :: UserName
  , unrPassword :: Text
  } deriving (Generic, FromJSON, ToJSON)

data UserNewResponse
  = UserNewSuccess UserName Text
  | UserNewFailed Text
  deriving (Generic, ToJSON)

data UserNameResponse = UserNameResponse
  { unrName :: UserName
  } deriving (Generic, ToJSON)

data LoginRequest = LoginRequest
  { lrUserName :: UserName
  , lrPassword :: Text
  } deriving (Generic, FromJSON, ToJSON)

data LoginResponse
  = LoginSuccess UserName Text
  | LoginFailed Text
  deriving (Generic, ToJSON)

data LogoutResponse = LogoutResponse
  deriving (Generic, ToJSON)

data UserExistsRequest = UserExistsRequest
  { uerName :: UserName
  } deriving (Generic, FromJSON, ToJSON)
