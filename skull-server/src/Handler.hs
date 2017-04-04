{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeOperators              #-}
-- |

module Handler
  ( HandlerT (..)
  , HandlerProtectedT
  , transform
  , runHandlerT
  ) where

import           Control.Monad.Except       (MonadError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader (..), ReaderT, asks)
import           Control.Monad.Trans.Class  (MonadTrans (..))
import qualified Data.ByteString.Lazy       as ByteString.Lazy
import qualified Data.Text.Encoding         as Text
import           Database.PostgreSQL.Simple (Connection)
import           Diener                     (DienerT (..), LogEnv (logEnv),
                                             runDienerT, throwError)
import qualified Opaleye
import           Servant                    ((:~>) (..), Handler,
                                             ServantErr (..), enter, err400,
                                             err401, err403, err500)
import           Servant.Utils.Enter        (Enter)

import           Auth.Types                 (UserInfo)
import qualified Database.Class             as Db
import           Types                      (AppEnv (..), AppError (..), Env)

newtype HandlerT m a = HandlerT { unHandlerT :: DienerT AppError AppEnv m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError AppError)

type HandlerProtectedT m = HandlerT (ReaderT UserInfo m)

runHandlerT :: Env -> HandlerT m a -> m (Either AppError a)
runHandlerT env = runDienerT env . unHandlerT

transform :: (Enter h (HandlerT IO :~> Handler) s)
          => Env -> h -> s
transform env =
    enter $ Nat $ \action ->
      liftIO (runHandlerT env action)
        >>= either
          (throwError . appErrToServantErr)
          pure
  where
    appErrToServantErr :: AppError -> ServantErr
    appErrToServantErr = \case
        ErrUser msg         -> err400 { errBody = toBS msg }
        ErrBug msg          -> err500 { errBody = toBS msg }
        ErrDatabase msg     -> err500 { errBody = toBS msg }
        ErrUnauthorized msg -> err401 { errBody = toBS msg }
        ErrForbidden msg    -> err403 { errBody = toBS msg }
      where
        toBS = ByteString.Lazy.fromStrict . Text.encodeUtf8

getDbConnection :: Monad m => HandlerT m Connection
getDbConnection = HandlerT $ asks $ envDbConnection . logEnv

instance MonadReader UserInfo m => MonadReader UserInfo (HandlerT m) where
  ask = lift ask

instance MonadTrans HandlerT where
  lift = HandlerT . lift

instance MonadIO m => Db.Insert (HandlerT m) where
  insertMany table rows f = do
    connection <- getDbConnection
    liftIO $ Opaleye.runInsertManyReturning connection table rows f
  insertMany_ table rows = do
    connection <- getDbConnection
    () <$ liftIO (Opaleye.runInsertMany connection table rows)

instance MonadIO m => Db.Read (HandlerT m) where
  getByQuery query = do
    connection <- getDbConnection
    liftIO $ Opaleye.runQuery connection query

instance MonadIO m => Db.Delete (HandlerT m) where
  deleteWhere table predicate = do
    connection <- getDbConnection
    () <$ liftIO (Opaleye.runDelete connection table predicate)

instance MonadIO m => Db.Update (HandlerT m) where
  updateWhere table f predicate = do
    connection <- getDbConnection
    () <$ liftIO (Opaleye.runUpdate connection table f predicate)
