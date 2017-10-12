{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Auth
  ( authHandler
  ) where

import           Control.Lens           ((.~), (^.))
import           Control.Monad.Except   (ExceptT (..), lift, runExceptT)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (ReaderT (..))
import           Data.Text              (Text)
import qualified Data.Time.Clock        as Clock
import           Database.Gerippe       (Entity (..))
import           Diener                 (DienerT (..), throwError)
import           Servant                ((:~>) (..), enter)
import           Servant.Utils.Enter    (Enter)

import           Auth.Types             (AuthToken (..), UserInfo (..),
                                         sessionLength)
import qualified Database.Class         as Db
import           Handler                (HandlerProtectedT, HandlerT (..),
                                         runHandlerT)
import           HttpApp.Model          (EntityField (..), Session (..),
                                         User (..))
import           HttpApp.User.Types     (SessionKey)
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
lookupSession key = runExceptT $ do
    (Entity sKey Session{..}, Entity uId User{..}) <- sessionQuery
    now <- liftIO Clock.getCurrentTime
    if now > sessionExpiry
        then do lift $ Db.delete sKey
                throwError "session expired"
        else do
          let newExpiry = Clock.addUTCTime sessionLength now
          lift $ Db.update sKey $ Session
            { sessionFkUser = sessionFkUser
            , sessionExpiry = newExpiry
            , sessionKey = sessionKey
            }
          pure UserInfo
            { _uiUserId     = uId
            , _uiUserName   = userName
            , _uiEmail      = userEmail
            , _uiSessionKey = sessionKey
            }
  where
    sessionQuery =
      lift (Db.joinMTo1Where' SessionFkUser UserId SessionKey key) >>= \case
        []    -> throwError "session not found"
        _:_:_ -> throwError "multiple sessions found"
        [res] -> pure res
