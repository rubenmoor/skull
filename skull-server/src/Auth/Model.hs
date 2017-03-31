module Auth.Model where

import           Data.Int              (Int64)
import           Data.Text             (Text)
import           Data.Time.Clock       (UTCTime)
import           Util.Base64           (Base64)

import           Database.Schema.Types

type UserName = Text
type Email = Text
type PwHash = Base64

type UserId = UserId' Int64
type User = User' UserId UserName PwHash Email

type SessionId = SessionId' Int64
type SessionKey = SessionKey' Base64
type Session = Session' SessionId UserId SessionKey UTCTime
