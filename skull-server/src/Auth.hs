{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Auth
  ( authHandler
  ) where

import           Control.Monad.Except   (ExceptT (..), lift, runExceptT)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (ReaderT (..))
import           Data.Text              (Text)
import qualified Data.Time.Clock        as Clock
import           Database.Esqueleto     (Entity (..), InnerJoin (..), from, set,
                                         val, where_, (&&.), (=.), (==.), (^.))
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
    (Entity sId Session{..}, Entity uId User{..}) <- sessionQuery
    now <- liftIO Clock.getCurrentTime
    if now > sessionExpiry
        then do lift $ Db.deleteByKey sId
                throwError "session expired"
        else do
          let newExpiry = Clock.addUTCTime sessionLength now
          lift $ Db.update $ \s -> do
            set s [ SessionExpiry =. val newExpiry ]
            where_ $ s ^. SessionId ==. val sId
          pure UserInfo
            { _uiUserId     = uId
            , _uiUserName   = userName
            , _uiEmail      = userEmail
            , _uiSessionKey = sessionKey
            }
  where
    sessionQuery = do
      ls <- lift $ Db.select $ from $ \(s `InnerJoin` u) -> do
        where_ $ (s ^. SessionKey ==. val key)
             &&. (u ^. UserId ==. s ^. SessionFkUser)
        pure (s, u)
      case ls of
        []    -> throwError "session not found"
        _:_:_ -> throwError "multiple sessions found"
        [res] -> pure res
