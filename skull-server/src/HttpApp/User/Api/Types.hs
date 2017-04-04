{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module HttpApp.User.Api.Types where

import           Data.Aeson         (FromJSON, ToJSON)
import           Data.Text          (Text)
import           GHC.Generics       (Generic)

import           HttpApp.User.Model (SessionKey)
import           HttpApp.User.Types (UserName)

data UserNewRequest = UserNewRequest
  { unrUserName :: UserName
  , unrPassword :: Text
  } deriving (Generic, FromJSON)

data UserNewResponse
  = UserNewSuccess UserName SessionKey
  | UserNewFailed Text
  deriving (Generic, ToJSON)

data LoginRequest = LoginRequest
  { lrUserName :: UserName
  , lrPassword :: Text
  } deriving (Generic, FromJSON)

data LoginResponse
  = LoginSuccess UserName SessionKey
  | LoginFailed Text
  deriving (Generic, ToJSON)
