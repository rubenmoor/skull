{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Auth
  ( authHandler
  ) where

import           Control.Lens           ((.~), (^.))
import           Control.Monad.Except   (ExceptT (..))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (ReaderT (..))
import           Data.Text              (Text)
import qualified Data.Time.Clock        as Clock
import           Diener                 (DienerT (..), throwError)
import           Servant                ((:~>) (..), enter)
import           Servant.Utils.Enter    (Enter)

import           Auth.Types             (AuthToken (..), UserInfo (..),
                                         sessionLength)
import qualified Database.Class         as Db
import           Database.Common        (deleteSession, updateSession)
import qualified Database.Query         as Query
import           Database.Schema.Types
import           Handler                (HandlerProtectedT, HandlerT (..),
                                         runHandlerT)
import           HttpApp.User.Model     (Session, SessionKey, User)
import           Opaleye                (pgUTCTime)
import           Types                  (AppError (ErrForbidden, ErrUnauthorized))

-- middleware
authHandler :: (Enter h1 (HandlerProtectedT IO :~> HandlerT IO) h2)
            => h1 -> Maybe AuthToken -> h2
authHandler h mAuth =
  flip enter h $ Nat $ \action -> do
    AuthToken sKey <- maybe (throwError $ ErrForbidden "Missing auth token")
                            pure
                            mAuth
    userInfo <- lookupSession sKey >>= either (throwError . ErrUnauthorized) pure
    HandlerT $ DienerT $ ExceptT $ ReaderT $ \env ->
      runReaderT (runHandlerT env action) userInfo

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
