{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module HttpApp.User.Handler where

import           Control.Lens
import           Control.Monad.Except   (ExceptT (ExceptT), MonadError,
                                         runExceptT, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, asks)
import           Data.Foldable          (for_)
import qualified Data.Time.Clock        as Clock
import           Database.Gerippe       (Entity (..))
import           Servant                ((:<|>) (..), ServerT)
import           System.Entropy         (getEntropy)
import           TextShow               (showt)

import           Auth.Types             (UserInfo, sessionLength, uiUserId,
                                         uiUserName)
import qualified Database.Class         as Db
import           Handler                (HandlerProtectedT, HandlerT)
import           HttpApp.Model          (EntityField (..), Session (..),
                                         User (..), UserId)
import qualified HttpApp.User.Api       as Api
import           HttpApp.User.Api.Types
import           HttpApp.User.Types     (SessionKey, mkPwHash, verifyPassword)
import           Types                  (AppError (..))
import qualified Util.Base64            as Base64

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
  Db.getOneWhere UserName unrUserName >>= \case
    Right _ -> pure $ UserNewFailed "username already exists"
    Left  _ -> do
      pwHash <- mkPwHash unrPassword
      uId <- Db.insert User
        { userName = unrUserName
        , userPwHash = pwHash
        , userEmail = Nothing
        }
      sKey <- createSession uId
      pure $ UserNewSuccess unrUserName $ showt sKey

userExists :: (Db.Read m, Monad m)
           => UserExistsRequest
           -> m Bool
userExists UserExistsRequest{..} =
  not . null <$> Db.getWhere UserName uerName

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
      ExceptT $ over _Left (\_ -> "user name unknown") <$>
        Db.getOneWhere UserName lrUserName
    checkPassword_ (Entity _ User{..}) =
      if verifyPassword lrPassword userPwHash
        then pure ()
        else throwError "wrong password"

    getSession uId = do
      -- when logged in: log out first
      ss <- Db.getWhere SessionFkUser uId
      for_ ss $ \(Entity sId _) -> Db.delete sId
      -- and create brand-new session
      createSession uId

logout :: (MonadError AppError m, Db.Read m, Db.Delete m, MonadReader UserInfo m)
       => m LogoutResponse
logout = do
  uId <- asks $ view uiUserId
  Db.getOneWhere SessionFkUser uId >>= \case
    Left msg              -> throwError $ ErrBug msg
    Right (Entity sKey _) -> Db.delete sKey
  pure LogoutResponse

getName :: MonadReader UserInfo m
     => m UserNameResponse
getName = do
  name <- asks $ view uiUserName
  pure UserNameResponse { unrName = name }

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
