{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Auth.Types where

import           Control.Lens
import           Data.Aeson         (FromJSON)
import           GHC.Generics       (Generic)
import           Servant.API        (Header)
import           Servant.PureScript (jsonParseHeader, jsonParseUrlPiece)
import           Web.HttpApiData    (FromHttpApiData (..))

import qualified Data.Time.Clock    as Clock
import           HttpApp.User.Model (SessionKey, UserId)
import           HttpApp.User.Types (Email, UserName)

type AuthProtect = Header "AuthToken" AuthToken

data AuthToken
  = AuthToken SessionKey
  deriving (Generic, FromJSON)

instance FromHttpApiData AuthToken where
  parseUrlPiece = jsonParseUrlPiece
  parseHeader   = jsonParseHeader

data UserInfo = UserInfo
  { _uiUserId     :: UserId
  , _uiUserName   :: UserName
  , _uiEmail      :: Maybe Email
  , _uiSessionKey :: SessionKey
  }

sessionLength :: Clock.NominalDiffTime
sessionLength = 60 * 60 * 24 * 60 -- 2 months

makeLenses ''UserInfo
