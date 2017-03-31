module Database.Schema where

import           Opaleye

import           Database.Schema.Types

type UserId = UserId' (Column PGInt8)
type UserW = User' (UserId' (Maybe (Column PGInt8)))
                   (Column PGText)
                   (Column PGText)
                   (Column PGText)
type UserR = User' UserId
                   (Column PGText)
                   (Column PGText)
                   (Column PGText)

users :: Table UserW UserR
users = Table "users" (pUser User
  { _userId     = pUserId $ UserId $ optional "id"
  , _userName   = required "name"
  , _userPwHash = required "pwHash"
  , _userEmail  = required "email"
  })

type BotKeyId = BotKeyId' (Column PGInt8)
type BotKeyW = BotKey' (BotKeyId' (Maybe (Column PGInt8)))
                       UserId
                       (Column PGText)
                       (Column PGText)

type BotKeyR = BotKey' BotKeyId
                       UserId
                       (Column PGText)
                       (Column PGText)

botKeys :: Table BotKeyW BotKeyR
botKeys = Table "botKeys" (pBotKey BotKey
  { _botKeyId     = pBotKeyId $ BotKeyId $ optional "id"
  , _botKeyFkUser = pUserId $ UserId $ required "fkUser"
  , _botKeyLabel  = required "label"
  , _botKeySecret = required "secret"
  })

type SessionId = SessionId' (Column PGInt8)
type SessionKey = SessionKey' (Column PGText)
type SessionW = Session' (SessionId' (Maybe (Column PGInt8)))
                         UserId
                         SessionKey
                         (Column PGTimestamptz)
type SessionR = Session' SessionId
                         UserId
                         SessionKey
                         (Column PGTimestamptz)

sessions :: Table SessionW SessionR
sessions = Table "sessions" (pSession Session
  { _sessionId     = pSessionId $ SessionId $ optional "id"
  , _sessionFkUser = pUserId $ UserId $ required "fkUser"
  , _sessionKey    = pSessionKey $ SessionKey $ required "key"
  , _sessionExpiry = required "expiry"
  })
