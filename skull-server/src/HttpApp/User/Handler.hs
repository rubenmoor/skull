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
import           Handler                             (HandlerProtectedT,
                                                      HandlerT)
import           HttpApp.Model                       (EntityField (..),
                                                      Session (..), User (..),
                                                      UserId)
import qualified HttpApp.User.Api                    as Api
import           HttpApp.User.Api.Types
import           HttpApp.User.Types                  (SessionKey, UserName,
                                                      mkPwHash, verifyPassword)
import           Types                               (AppError (..))

public :: ServerT Api.Public (HandlerT IO)
public =
       userNew
  :<|> userExists
  :<|> login

protected :: ServerT Api.Protected (HandlerProtectedT IO)
protected =
       logout
  :<|> getName

userNew :: (MonadIO m, Db.Read m, Db.Insert m)
        => UserNewRequest
        -> m UserNewResponse
userNew UserNewRequest{..} =
  null <$> getUsersByName _unrUserName >>= \case
    False -> pure $ UserNewFailed "username already exists"
    True  -> do
      pwHash <- mkPwHash _unrPassword
      uId <- Db.insert User
        { userName = _unrUserName
        , userPwHash = pwHash
        , userEmail = Nothing
        }
      sKey <- createSession uId
      pure $ UserNewSuccess _unrUserName $ showt sKey

userExists :: (Db.Read m, Monad m)
           => UserExistsRequest
           -> m Bool
userExists UserExistsRequest{..} =
  not . null <$> getUsersByName _uerName

login :: (Db.Read m, Db.Delete m, Db.Insert m, MonadIO m)
          => LoginRequest
          -> m LoginResponse
login LoginRequest{..} =
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
      lift (getUsersByName _lrUserName) >>= \case
        []    -> throwError "username unknown"
        _:_:_ -> throwError "database inconsistency: multiple users"
        [u]   -> pure u
    checkPassword_ (Entity _ User{..}) =
      if verifyPassword _lrPassword userPwHash
        then pure ()
        else throwError "wrong password"

    getSession uId = do
      -- when logged in: log out first
      Db.delete $ from $ \s -> where_ $ s ^. SessionFkUser ==. val uId
      -- and create brand-new session
      createSession uId

logout :: (MonadError AppError m, Db.Read m, Db.Delete m, MonadReader UserInfo m)
       => m LogoutResponse
logout = do
  uId <- asks $ view uiUserId
  n <- Db.deleteCount $ from $ \s -> where_ $ s ^. SessionFkUser ==. val uId
  if n == 0
    then throwError $ ErrBug "not logged in"
    else pure LogoutResponse

getName :: MonadReader UserInfo m
     => m UserNameResponse
getName = do
  name <- asks $ view uiUserName
  pure UserNameResponse { _unrName = name }

--

createSession
  :: (MonadIO m, Db.Insert m)
  => UserId
  -> m SessionKey
createSession sessionFkUser = do
  sessionKey <- Base64.encode <$> liftIO (getEntropy 32)
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
