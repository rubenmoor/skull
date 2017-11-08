{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}

module Auth
  ( module Auth.Types
  , hoistAuthApp
  , hoistAuthAppBotKey
  ) where

import           Control.Monad.Except   (MonadError, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (ReaderT (..))
import qualified Data.Time.Clock        as Clock
import           Database.Esqueleto     (Entity (..), InnerJoin (..), from, set,
                                         val, where_, (&&.), (=.), (==.), (^.))
import qualified Servant
import           Servant.Utils.Enter    ((:~>) (..), Enter, enter)

import           Auth.Types
import qualified Database.Class         as Db
import           Handler.Types          (AppEnv (..), AppError (..),
                                         HandlerAuthT, HandlerT, runHandlerT,
                                         toServantError)
import qualified HttpApp.BotKey.Types   as BotKey
import           HttpApp.Model          (BotKey (..), EntityField (..),
                                         Session (..), User (..))
import           HttpApp.User.Types     (SessionKey)

-- middleware

hoistAuthApp
  :: Enter h (HandlerAuthT IO) Servant.Handler s
  => AppEnv
  -> h
  -> Maybe AuthToken
  -> s
hoistAuthApp env a mAuth =
    flip enter a $ NT $ fromAuthApp env mAuth

fromAuthApp
  :: AppEnv
  -> Maybe AuthToken
  -> HandlerAuthT IO a
  -> Servant.Handler a
fromAuthApp env mAuth action = do
  eUserInfo <- liftIO $ runHandlerT env $ getAuthEnv mAuth
  userInfo <- either (throwError . toServantError) pure eUserInfo
  e <- liftIO $ runReaderT (runHandlerT env action) userInfo
  either (throwError . toServantError) pure e

getAuthEnv
  :: Maybe AuthToken
  -> HandlerT IO UserInfo
getAuthEnv mAuth = do
  auth <- maybe (throwError $ ErrForbidden "Missing auth token")
                pure
                mAuth
  sKey <- case auth of
    AuthSession sKey -> pure sKey
    AuthBotKey  _    -> throwError $ ErrForbidden "BotKey auth not supported here"
  lookupSession sKey

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

hoistAuthAppBotKey
  :: Enter h (HandlerAuthT IO) Servant.Handler s
  => AppEnv
  -> h
  -> Maybe AuthToken
  -> s
hoistAuthAppBotKey env a mAuth =
  flip enter a $ NT $ fromAuthAppBotKey env mAuth

fromAuthAppBotKey
  :: AppEnv
  -> Maybe AuthToken
  -> HandlerAuthT IO a
  -> Servant.Handler a
fromAuthAppBotKey env mAuth action = do
  eUserInfo <- liftIO $ runHandlerT env $ getAuthEnvBotKey mAuth
  userInfo <- either (throwError . toServantError) pure eUserInfo
  e <- liftIO $ runReaderT (runHandlerT env action) userInfo
  either (throwError . toServantError) pure e

getAuthEnvBotKey
  :: Maybe AuthToken
  -> HandlerT IO UserInfo
getAuthEnvBotKey mAuth = do
  auth <- maybe (throwError $ ErrForbidden "Missing auth token")
                pure
                mAuth
  case auth of
    AuthSession sKey   -> lookupSession sKey
    AuthBotKey  secret -> lookupBotKey secret

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
