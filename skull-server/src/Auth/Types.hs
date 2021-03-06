{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Auth.Types where

import           Control.Lens
import           Data.Aeson           (FromJSON)
import           GHC.Generics         (Generic)
import           Servant.API          (Header)
import           Servant.PureScript   (jsonParseHeader, jsonParseUrlPiece)
import           Web.HttpApiData      (FromHttpApiData (..))

import qualified Data.Time.Clock      as Clock
import           HttpApp.BotKey.Types (BotKey)
import           HttpApp.BotKey.Types as BotKey
import           HttpApp.Model        (UserId)
import           HttpApp.User.Types   (Email, SessionKey, UserName)

type AuthProtect = Header "AuthToken" AuthToken

data AuthToken
  = AuthSession SessionKey
  | AuthBotKey  BotKey.Secret
  deriving (Generic, FromJSON)

instance FromHttpApiData AuthToken where
  parseUrlPiece = jsonParseUrlPiece
  parseHeader   = jsonParseHeader

data UserInfo = UserInfo
  { _uiUserId       :: UserId
  , _uiUserName     :: UserName
  , _uiEmail        :: Maybe Email
  , _uiActiveBotKey :: Maybe BotKey -- Nothing => PlayNow
  }

sessionLength :: Clock.NominalDiffTime
sessionLength = 60 * 60 * 24 * 60 -- 2 months

makeLenses ''UserInfo
