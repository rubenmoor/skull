{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module HttpApp.User.Types where

import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Crypto.PasswordStore                 (makePassword,
                                                       verifyPassword)
import           Data.Text                            (Text)
import qualified Data.Text.Encoding                   as Text
import           Database.PostgreSQL.Simple.FromField (FromField (..))
import           Opaleye                              (PGText, QueryRunnerColumnDefault (..),
                                                       fieldQueryRunnerColumn)
import qualified Text.Email.Validate                  as Email
import           TextShow                             (TextShow (..), fromText)

import           Util.Base64                          (Base64)
import qualified Util.Base64                          as Base64

type UserName = Text

type PwHash = Base64

mkPwHash :: MonadIO m => Text -> m PwHash
mkPwHash str =
  liftIO $ Base64.fromByteStringUnsafe <$>
    makePassword (Text.encodeUtf8 str) 17

verifyPassword :: Text -> PwHash -> Bool
verifyPassword str pwHash =
  Crypto.PasswordStore.verifyPassword (Text.encodeUtf8 str)
                                      (Base64.toByteString pwHash)

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
