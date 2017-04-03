{-# LANGUAGE MultiParamTypeClasses #-}
module Auth.Model where

import           Data.Int                             (Int64)
import           Data.Text                            (Text)
import qualified Data.Text.Encoding                   as Text
import           Data.Time.Clock                      (UTCTime)
import           Database.PostgreSQL.Simple.FromField (FromField (..))
import           Opaleye                              (PGText, QueryRunnerColumnDefault (..),
                                                       fieldQueryRunnerColumn)
import qualified Text.Email.Validate                  as Email
import           TextShow

import           Database.Schema.Types
import           Util.Base64                          (Base64)

type UserName = Text
type PwHash = Base64

type UserId = UserId' Int64
type User = User' UserId UserName PwHash (Maybe Email)

type SessionId = SessionId' Int64
type SessionKey = SessionKey' Base64
type Session = Session' SessionId UserId SessionKey UTCTime

newtype Email = Email { unEmail :: Text }

instance TextShow Email where
  showb = fromText . unEmail

instance FromField Email where
  fromField f mdata = Email <$> fromField f mdata

instance QueryRunnerColumnDefault PGText Email where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

mkEmail :: Text -> Either Text Email
mkEmail str =
  if Email.isValid (Text.encodeUtf8 str)
      then Right $ Email str
      else Left "not a valid address string"
