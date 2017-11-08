{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module HttpApp.User.Handler where

import           Control.Lens                        (view)
import           Control.Monad.Except                (MonadError, runExceptT,
                                                      throwError)
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Control.Monad.Reader                (MonadReader, asks)
import           Control.Monad.Trans.Class           (lift)
import qualified Data.Time.Clock                     as Clock
import           Servant                             ((:<|>) (..), ServerT)
import           System.Entropy                      (getEntropy)
import           TextShow                            (showt)

import           Auth.Types                          (UserInfo, sessionLength,
                                                      uiUserId, uiUserName)
import qualified Data.ByteString.Base64.URL.Extended as Base64
import qualified Database.Class                      as Db
import           Database.Esqueleto                  (Entity (..), from, val,
                                                      where_, (==.), (^.))
import           Handler.Types                       (AppError (..),
                                                      HandlerAuthT, HandlerT)
import           HttpApp.Model                       (EntityField (..),
                                                      Session (..), User (..),
                                                      UserId)
import qualified HttpApp.User.Api                    as Api
import           HttpApp.User.Api.Types
import           HttpApp.User.Types                  (SessionKey, UserName,
                                                      mkPwHash, verifyPassword)

public :: ServerT Api.Public (HandlerT IO)
public =
       userNew
  :<|> userExists
  :<|> login

protected :: ServerT Api.Protected (HandlerAuthT IO)
protected =
       logout
  :<|> getName

userNew :: (MonadIO m, Db.Read m, Db.Insert m)
        => UserNewRq
        -> m UserNewResp
userNew UserNewRq{..} =
  null <$> getUsersByName _nrqUserName >>= \case
    False -> pure $ NewFailed "username already exists"
    True  -> do
      pwHash <- mkPwHash _nrqPassword
      uId <- Db.insert User
        { userName = _nrqUserName
        , userPwHash = pwHash
        , userEmail = Nothing
        }
      sKey <- createSession uId
      pure $ NewSuccess _nrqUserName $ showt sKey

userExists :: (Db.Read m, Monad m)
           => UserExistsRq
           -> m Bool
userExists UserExistsRq{..} =
  not . null <$> getUsersByName _erqUserName

login :: (Db.Read m, Db.Delete m, Db.Insert m, MonadIO m)
          => LoginRq
          -> m LoginResp
login LoginRq{..} =
    checkLogin >>= \case
      Left  err  -> pure $ LoginFailed err
      Right (Entity uId User{..}) -> do
        sKey <- getSession uId
        pure $ LoginSuccess userName $ showt sKey
  where
    checkLogin = runExceptT $ do
      user <- getUser
      checkPassword_ user
      pure user
    getUser =
      lift (getUsersByName _lrqUserName) >>= \case
        []    -> throwError "username unknown"
        _:_:_ -> throwError "database inconsistency: multiple users"
        [u]   -> pure u
    checkPassword_ (Entity _ User{..}) =
      if verifyPassword _lrqPassword userPwHash
        then pure ()
        else throwError "wrong password"

    getSession uId = do
      -- when logged in: log out first
      Db.delete $ from $ \s -> where_ $ s ^. SessionFkUser ==. val uId
      -- and create brand-new session
      createSession uId

logout :: (MonadError AppError m, Db.Read m, Db.Delete m, MonadReader UserInfo m)
       => m LogoutResp
logout = do
  uId <- asks $ view uiUserId
  n <- Db.deleteCount $ from $ \s -> where_ $ s ^. SessionFkUser ==. val uId
  if n == 0
    then throwError $ ErrBug "not logged in"
    else pure LogoutResp

getName :: MonadReader UserInfo m
     => m UserNameResp
getName = do
  name <- asks $ view uiUserName
  pure UserNameResp { _nrespUserName = name }

--

createSession
  :: (MonadIO m, Db.Insert m)
  => UserId
  -> m SessionKey
createSession sessionFkUser = do
  sessionKey <- liftIO $ Base64.encode <$> getEntropy 32
  sessionExpiry <- liftIO $ Clock.addUTCTime sessionLength <$> Clock.getCurrentTime
  Db.insert_ Session{..}
  pure sessionKey

getUsersByName
  :: Db.Read m
  => UserName
  -> m [Entity User]
getUsersByName str =
  Db.select $ from $ \u -> do
    where_ $ u ^. UserName ==. val str
    pure u
