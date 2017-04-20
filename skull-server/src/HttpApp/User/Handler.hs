{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module HttpApp.User.Handler where

import           Control.Lens
import           Control.Monad.Except   (ExceptT (ExceptT), MonadError,
                                         runExceptT, throwError)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, asks)
import           Data.Foldable          (for_)
import           Data.Text              (Text)
import           Servant                ((:<|>) (..), ServerT)
import           TextShow               (showt)

import           Auth.Types             (UserInfo, uiUserId, uiUserName)
import           Database.Adaptor       (mkUser)
import qualified Database.Class         as Db
import           Database.Common        (createSession, deleteSession)
import qualified Database.Query         as Query
import           Database.Schema        (users)
import           Database.Schema.Types
import           Handler                (HandlerProtectedT, HandlerT)
import qualified HttpApp.User.Api       as Api
import           HttpApp.User.Api.Types
import           HttpApp.User.Model     (Session, User)
import           HttpApp.User.Types     (UserName, mkPwHash, verifyPassword)
import           Types                  (AppError (..))

public :: ServerT Api.Public (HandlerT IO)
public =
       userNew
  :<|> userExists
  :<|> login

protected :: ServerT Api.Protected (HandlerProtectedT IO)
protected =
       logout
  :<|> name

userNew :: (MonadIO m, Db.Read m, Db.Insert m)
        => UserNewRequest
        -> m UserNewResponse
userNew UserNewRequest { unrUserName = name
                       , unrPassword = password
                       } =
  Db.getOneByQuery (Query.userByUserName name) >>= \case
    Right (_ :: User) -> pure $ UserNewFailed "username already exists"
    Left  _           -> do
      pwHash <- mkPwHash password
      uId <- Db.insert users (mkUser name pwHash Nothing) (view userId)
      SessionKey key <- createSession uId
      pure $ UserNewSuccess name $ showt key


userExists :: Db.Read m
           => UserName
           -> m Bool
userExists name =
  not . null <$> (Db.getByQuery (Query.userByUserName name) :: Db.Read m => m [User])

login :: (Db.Read m, Db.Delete m, Db.Insert m, MonadIO m)
          => LoginRequest
          -> m LoginResponse
login LoginRequest { lrUserName = name
                   , lrPassword = password
                   } =
    checkLogin >>= \case
      Left  err    -> pure $ LoginFailed err
      Right user -> do
        SessionKey key <- getSession $ user ^. userId
        pure $ LoginSuccess (user ^. userName) $ showt key
  where
    checkLogin = runExceptT $ do
      user <- getUser
      checkPassword_ (user :: User)
      pure user
    getUser = ExceptT $ over _Left (\_ -> "user name unknown") <$>
      Db.getOneByQuery (Query.userByUserName name)
    checkPassword_ user =
      if verifyPassword password (user ^. userPwHash)
        then pure ()
        else throwError "wrong password"

    getSession uId = do
      -- when logged in: log out first
      eSession <- Db.getOneByQuery (Query.sessionByUserId uId)
      for_ (eSession :: Either Text Session) $ \session ->
        deleteSession $ session ^. sessionId
      -- and create brand-new session
      createSession uId

logout :: (MonadError AppError m, Db.Read m, Db.Delete m, MonadReader UserInfo m)
       => m ()
logout = do
  uId <- asks $ view uiUserId
  Db.getOneByQuery (Query.sessionByUserId uId) >>= \case
    Left msg -> throwError $ ErrBug msg
    Right session -> deleteSession $ (session :: Session) ^. sessionId

name :: MonadReader UserInfo m
     => m UserName
name = asks $ view uiUserName
