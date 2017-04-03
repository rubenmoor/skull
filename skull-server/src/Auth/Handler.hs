{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Auth.Handler where

import           Control.Lens
import           Control.Monad.Except   (ExceptT (ExceptT), MonadError,
                                         runExceptT, throwError)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Foldable          (for_)
import           Data.Text              (Text)
import           Servant                ((:<|>) (..), ServerT)

import           Auth                   (mkPwHash, verifyPassword)
import qualified Auth.Api               as Api
import           Auth.Api.Types
import           Auth.Model             (Session, User)
import           Auth.Types             (UserInfo, uiUserId)
import           Database.Adaptor       (mkUser)
import qualified Database.Class         as Db
import           Database.Common        (createSession, deleteSession)
import qualified Database.Query         as Query
import           Database.Schema        (users)
import           Database.Schema.Types
import           Handler.Types          (HandlerT)
import           Types                  (AppError (..))

handlers :: ServerT Api.Routes (HandlerT IO)
handlers =
       userNew
  :<|> authLogin
  :<|> authLogout

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
      uId <- Db.insert users (mkUser name pwHash "") (view userId)
      key <- createSession uId
      pure $ UserNewSuccess name key

authLogin :: (Db.Read m, Db.Delete m, Db.Insert m, MonadIO m)
          => LoginRequest
          -> m LoginResponse
authLogin LoginRequest { lrUserName = name
                       , lrPassword = password
                       } =
    checkLogin >>= \case
      Left  err    -> pure $ LoginFailed err
      Right user -> do
        key <- getSession $ user ^. userId
        pure $ LoginSuccess (user ^. userName) key
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

authLogout :: (MonadError AppError m, Db.Read m, Db.Delete m)
           => UserInfo
           -> m ()
authLogout userInfo =
  Db.getOneByQuery (Query.sessionByUserId $ userInfo ^. uiUserId) >>= \case
    Left msg -> throwError $ ErrBug msg
    Right session -> deleteSession $ (session :: Session) ^. sessionId
