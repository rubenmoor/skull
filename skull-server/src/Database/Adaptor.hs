{-# LANGUAGE RecordWildCards #-}

module Database.Adaptor where

import           Data.Time.Clock       (UTCTime)
import           Opaleye
import           TextShow

import           Database.Schema
import           Database.Schema.Types
import qualified HttpApp.User.Model    as Model
import           HttpApp.User.Types    (Email, PwHash, UserName)

pgUserId :: Model.UserId -> UserId
pgUserId = UserId . pgInt8 . unUserId

mkUser :: UserName -> PwHash -> Maybe Email -> UserW
mkUser name pwHash email = User
  { _userId = UserId Nothing
  , _userName = pgStrictText name
  , _userPwHash = pgStrictText $ showt pwHash
  , _userEmail = maybeToNullable $ pgStrictText . showt <$> email
  }

pgSessionId :: Model.SessionId -> SessionId
pgSessionId = SessionId . pgInt8 . unSessionId

pgSessionKey :: Model.SessionKey -> SessionKey
pgSessionKey = SessionKey . pgStrictText . showt . unSessionKey

sessionRW :: SessionR -> SessionW
sessionRW Session {..} =
  Session (SessionId Nothing) _sessionFkUser _sessionKey _sessionExpiry

mkSession :: Model.UserId -> Model.SessionKey -> UTCTime -> SessionW
mkSession uId key expiry = Session
  { _sessionId     = SessionId Nothing
  , _sessionFkUser = pgUserId uId
  , _sessionKey    = pgSessionKey key
  , _sessionExpiry = pgUTCTime expiry
  }
