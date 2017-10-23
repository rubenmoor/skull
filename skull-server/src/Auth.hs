{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Auth
  ( authHandler
  , authHandlerBotKey
  ) where

import           Control.Monad.Except   (ExceptT (..), MonadError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (ReaderT (..))
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
import qualified HttpApp.BotKey.Types   as BotKey
import           HttpApp.Model          (BotKey (..), EntityField (..),
                                         Session (..), User (..))
import           HttpApp.User.Types     (SessionKey)
import           Types                  (AppError (ErrForbidden, ErrUnauthorized))

-- middleware

authHandler
  :: (Enter h1 (HandlerProtectedT IO :~> HandlerT IO) h2)
  => h1
  -> Maybe AuthToken
  -> h2
authHandler h mAuth =
  flip enter h $ Nat $ \action -> do
    auth <- maybe (throwError $ ErrForbidden "Missing auth token")
                  pure
                  mAuth
    sKey <- case auth of
      AuthSession sKey -> pure sKey
      AuthBotKey  _    -> throwError $ ErrForbidden "BotKey auth not supported here"
    userInfo <- lookupSession sKey
    HandlerT $ DienerT $ ExceptT $ ReaderT $ \env ->
      runReaderT (runHandlerT env action) userInfo

lookupSession :: (Db.Read m, Db.Delete m, Db.Update m, MonadIO m, MonadError AppError m)
              => SessionKey
              -> m UserInfo
lookupSession key = do
    (Entity sId Session{..}, Entity uId User{..}) <- sessionQuery
    now <- liftIO Clock.getCurrentTime
    if now > sessionExpiry
        then do Db.deleteByKey sId
                throwError $ ErrUnauthorized "session expired"
        else do
          let newExpiry = Clock.addUTCTime sessionLength now
          Db.update $ \s -> do
            set s [ SessionExpiry =. val newExpiry ]
            where_ $ s ^. SessionId ==. val sId
          pure UserInfo
            { _uiUserId     = uId
            , _uiUserName   = userName
            , _uiEmail      = userEmail
            , _uiActiveBotKey = Nothing
            }
  where
    sessionQuery = do
      ls <- Db.select $ from $ \(s `InnerJoin` u) -> do
        where_ $ (s ^. SessionKey ==. val key)
             &&. (u ^. UserId ==. s ^. SessionFkUser)
        pure (s, u)
      case ls of
        []    -> throwError $ ErrUnauthorized "session not found"
        _:_:_ -> throwError $ ErrUnauthorized "multiple sessions found"
        [res] -> pure res

-- authHandler that optionally allows authentification via botkey OR session

authHandlerBotKey
  :: (Enter h1 (HandlerProtectedT IO :~> HandlerT IO) h2)
  => h1
  -> Maybe AuthToken
  -> h2
authHandlerBotKey h mAuth =
  flip enter h $ Nat $ \action -> do
    auth <- maybe (throwError $ ErrForbidden "Missing auth token")
                  pure
                  mAuth
    userInfo <- case auth of
      AuthSession sKey   -> lookupSession sKey
      AuthBotKey  secret -> lookupBotKey secret
    HandlerT $ DienerT $ ExceptT $ ReaderT $ \env ->
      runReaderT (runHandlerT env action) userInfo

lookupBotKey
  :: (Db.Read m, MonadError AppError m)
  => BotKey.Secret
  -> m UserInfo
lookupBotKey secret = do
    (Entity _ BotKey{..}, Entity uId User{..}) <- botKeyQuery
    pure UserInfo
      { _uiUserId = uId
      , _uiUserName = userName
      , _uiEmail = userEmail
      , _uiActiveBotKey = Just BotKey.BotKey
          { _bkLabel = botKeyLabel
          , _bkSecret = botKeySecret
          }
      }
  where
    botKeyQuery = do
      ls <- Db.select $ from $ \(bk `InnerJoin` u) -> do
        where_ $ (bk ^. BotKeySecret ==. val secret)
             &&. (u ^. UserId ==. bk ^. BotKeyFkUser)
        pure (bk ,u)
      case ls of
        []    -> throwError $ ErrUnauthorized "botkey not found"
        _:_:_ -> throwError $ ErrUnauthorized "multiple botkeys found"
        [res] -> pure res
