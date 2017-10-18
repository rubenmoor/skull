{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module HttpApp.User.Types where

import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Crypto.PasswordStore                (makePassword,
                                                      verifyPassword)
import           Data.Proxy                          (Proxy (..))
import           Data.Text                           (Text)
import qualified Data.Text.Encoding                  as Text
import           Database.Persist.Sql                (PersistField (..),
                                                      PersistFieldSql (..))
import qualified Text.Email.Validate                 as Email
import           TextShow                            (TextShow (..), fromText)

import           Data.ByteString.Base64.URL.Extended (Base64)
import qualified Data.ByteString.Base64.URL.Extended as Base64

type UserName = Text
type PwHash = Base64
type SessionKey = Base64

mkPwHash :: MonadIO m => Text -> m PwHash
mkPwHash str =
  liftIO $ Base64.fromByteStringUnsafe <$>
    makePassword (Text.encodeUtf8 str) 17

verifyPassword :: Text -> PwHash -> Bool
verifyPassword str pwHash =
  Crypto.PasswordStore.verifyPassword (Text.encodeUtf8 str)
                                      (Base64.toByteString pwHash)

newtype Email = Email { unEmail :: Text }
  deriving (Eq, Ord)

instance TextShow Email where
  showb = fromText . unEmail

instance PersistField Email where
  toPersistValue = toPersistValue . unEmail
  fromPersistValue = fmap Email . fromPersistValue

instance PersistFieldSql Email where
  sqlType _ = sqlType (Proxy :: Proxy Text)

mkEmail :: Text -> Either Text Email
mkEmail str =
  if Email.isValid (Text.encodeUtf8 str)
      then Right $ Email str
      else Left "not a valid address string"
