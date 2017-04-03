{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
-- |

module Auth.Types where

import           Control.Lens                     (makeLenses)
import           Data.ByteString                  (ByteString)
import           Data.CaseInsensitive             (original)
import           Data.Text                        (Text)
import qualified Data.Text.Encoding               as Text
import qualified Data.Time.Clock                  as Clock
import           Network.HTTP.Types.Header        (HeaderName)
import           Network.Wai                      (Request)
import           Servant.API.Experimental.Auth    (AuthProtect)
import           Servant.Server.Experimental.Auth (AuthHandler, AuthServerData)

import           Auth.Model

type SessionProtect = AuthProtect "session-auth"
type AuthMiddleware = AuthHandler Request UserInfo
type instance AuthServerData SessionProtect = UserInfo

data UserInfo = UserInfo
  { _uiUserId     :: UserId
  , _uiUserName   :: UserName
  , _uiEmail      :: Maybe Email
  , _uiSessionKey :: SessionKey
  }

sessionLength :: Clock.NominalDiffTime
sessionLength = 60 * 60 * 24 * 60 -- 2 months

makeLenses ''UserInfo

sessionParam :: HeaderName
sessionParam = "auth-session"

sessionParamBStr :: ByteString
sessionParamBStr = original sessionParam

sessionParamStr :: Text
sessionParamStr = Text.decodeUtf8 sessionParamBStr
