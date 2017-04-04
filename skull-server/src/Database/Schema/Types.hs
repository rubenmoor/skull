{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}

module Database.Schema.Types where

import           Control.Lens               (makeLenses)
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           GHC.Generics               (Generic)

newtype UserId' a = UserId { unUserId :: a }
makeAdaptorAndInstance "pUserId" ''UserId'

data User' a b c d = User
  { _userId     :: a
  , _userName   :: b
  , _userPwHash :: c
  , _userEmail  :: d
  }
makeLenses ''User'
makeAdaptorAndInstance "pUser" ''User'

newtype BotKeyId' a = BotKeyId a
makeAdaptorAndInstance "pBotKeyId" ''BotKeyId'

data BotKey' a b c d = BotKey
  { _botKeyId     :: a
  , _botKeyFkUser :: b
  , _botKeyLabel  :: c
  , _botKeySecret :: d
  }
makeLenses ''BotKey'
makeAdaptorAndInstance "pBotKey" ''BotKey'

newtype SessionId' a = SessionId { unSessionId :: a }
makeAdaptorAndInstance "pSessionId" ''SessionId'

newtype SessionKey' a = SessionKey
  { unSessionKey :: a
  } deriving Generic
deriving instance ToJSON a => ToJSON (SessionKey' a)
deriving instance FromJSON a => FromJSON (SessionKey' a)
makeAdaptorAndInstance "pSessionKey" ''SessionKey'

data Session' a b c d = Session
  { _sessionId     :: a
  , _sessionFkUser :: b
  , _sessionKey    :: c
  , _sessionExpiry :: d
  }
makeLenses ''Session'
makeAdaptorAndInstance "pSession" ''Session'
