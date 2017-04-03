{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |

module Auth where

import           Control.Lens                     ((.~), (^.))
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Crypto.PasswordStore             (makePassword, verifyPassword)
import qualified Data.List                        as List
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text.Encoding               as Text
import qualified Data.Time.Clock                  as Clock
import           Diener                           (LogEnv, throwError)
import           Network.Wai                      (requestHeaders)
import           Servant.Server.Experimental.Auth (mkAuthHandler)

import           Auth.Model                       (PwHash, Session, SessionKey,
                                                   User)
import           Auth.Types                       (AuthMiddleware,
                                                   UserInfo (..), sessionLength,
                                                   sessionParam,
                                                   sessionParamStr)
import qualified Database.Class                   as Db
import           Database.Common                  (deleteSession, updateSession)
import qualified Database.Query                   as Query
import           Database.Schema.Types
import           Handler                          (transform)
import           Opaleye                          (pgUTCTime)
import           Types                            (AppError (ErrForbidden, ErrUnauthorized),
                                                   Env)
import qualified Util.Base64                      as Base64

mkPwHash :: MonadIO m => Text -> m PwHash
mkPwHash str =
  liftIO $ Base64.fromByteStringUnsafe <$> makePassword (Text.encodeUtf8 str) 17

verifyPassword :: Text -> PwHash -> Bool
verifyPassword str pwHash =
  Crypto.PasswordStore.verifyPassword (Text.encodeUtf8 str) (Base64.toByteString pwHash)

lookupSession :: (Db.Read m, Db.Delete m, Db.Update m, MonadIO m)
              => SessionKey
              -> m (Either Text UserInfo)
lookupSession key = do
    Db.getOneByQuery (Query.userAndSessionBySessionKey key)
      >>= either (\_ -> pure $ Left "session not found") getUserInfo
  where
    getUserInfo :: (Db.Read m, Db.Delete m, Db.Update m, MonadIO m)
                => (User, Session)
                -> m (Either Text UserInfo)
    getUserInfo (user, session) = do
      now <- liftIO Clock.getCurrentTime
      if now > session ^. sessionExpiry
        then Left "session expired" <$ deleteSession (session ^. sessionId)
        else do
          let newExpiry = pgUTCTime $ Clock.addUTCTime sessionLength now
          updateSession (session ^. sessionId) $ sessionExpiry .~ newExpiry
          pure $ Right UserInfo
            { _uiUserId     = user ^. userId
            , _uiUserName   = user ^. userName
            , _uiEmail      = user ^. userEmail
            , _uiSessionKey = session ^. sessionKey
            }

authHandler :: LogEnv Env -> AuthMiddleware
authHandler env = mkAuthHandler $ transform env $ \request ->
  case List.lookup sessionParam (requestHeaders request) of
    Nothing -> throwError $ ErrForbidden $ "Missing session parameter " <> sessionParamStr
    Just bs -> case Base64.fromByteString bs of
      Left  err -> throwError $ ErrUnauthorized err
      Right b64 -> lookupSession (SessionKey b64)
        >>= either (throwError . ErrUnauthorized) pure
