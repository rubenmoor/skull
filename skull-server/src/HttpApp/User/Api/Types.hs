{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module HttpApp.User.Api.Types where

import           Data.Aeson         (FromJSON, ToJSON)
import           Data.Text          (Text)
import           GHC.Generics       (Generic)

import           HttpApp.User.Types (UserName)

data UserNewRq = UserNewRq
  { _nrqUserName :: UserName
  , _nrqPassword :: Text
  } deriving (Generic, FromJSON, ToJSON)

data UserNewResp
  = NewSuccess UserName Text
  | NewFailed Text
  deriving (Generic, ToJSON)

data UserNameResp = UserNameResp
  { _nrespUserName :: UserName
  } deriving (Generic, ToJSON)

data LoginRq = LoginRq
  { _lrqUserName :: UserName
  , _lrqPassword :: Text
  } deriving (Generic, FromJSON, ToJSON)

data LoginResp
  = LoginSuccess UserName Text
  | LoginFailed Text
  deriving (Generic, ToJSON)

data LogoutResp = LogoutResp
  deriving (Generic, ToJSON)

data UserExistsRq = UserExistsRq
  { _erqUserName :: UserName
  } deriving (Generic, FromJSON, ToJSON)
