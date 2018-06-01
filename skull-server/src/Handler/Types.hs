{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Handler.Types
  ( AppEnv (..)
  , AppError (..)
  , hoistApp
  , HandlerAuthT
  , HandlerT
  , runHandlerT
  , toServantError
  ) where

import           Control.Exception.Lifted    (SomeException (..), catch)
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Except        (ExceptT (..), MonadError,
                                              runExceptT, throwError)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Logger        (MonadLogger (..), logError,
                                              toLogStr)
import           Control.Monad.Random        (MonadRandom)
import           Control.Monad.Reader        (MonadReader (..), ReaderT (..),
                                              asks, runReaderT)
import           Control.Monad.Trans.Class   (MonadTrans (..))
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..),
                                              defaultLiftBaseWith,
                                              defaultRestoreM)
import qualified Data.ByteString.Lazy        as ByteString.Lazy
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.Text.Encoding          as Text
import           Database.Esqueleto          (ConnectionPool)
import qualified Database.Esqueleto          as Esqueleto
import qualified Database.Persist            as Persist (delete)
import           Servant                     (ServantErr, err400, err401,
                                              err403, err500, errBody)
import qualified Servant                     (Handler (..))
import           Servant.Utils.Enter         ((:~>) (..), Enter, enter)

import           Auth.Types                  (UserInfo)
import qualified Database.Class              as Db
import           Logger                      (LogFunction)

data AppError
  = ErrUser Text
  | ErrBug  Text
  | ErrDatabase Text
  | ErrUnauthorized Text
  | ErrForbidden Text

data AppEnv = AppEnv
  { envDbConnection :: ConnectionPool
  , envLogFn        :: LogFunction
  }

newtype HandlerT m a = HandlerT
  { unHandlerT :: ReaderT AppEnv (ExceptT AppError m) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadError AppError)

deriving instance MonadBase IO m => MonadBase IO (HandlerT m)

deriving instance MonadRandom m => MonadRandom (HandlerT m)

instance MonadReader UserInfo m => MonadReader UserInfo (HandlerT m) where
    ask = lift ask
    local f a = HandlerT $ ReaderT $ \env -> ExceptT $ local f $ runHandlerT env a

instance MonadTrans HandlerT where
  lift = HandlerT . lift . lift

instance MonadTransControl HandlerT where
  type StT HandlerT m =  StT (ReaderT AppEnv) (StT (ExceptT AppError) m)
  liftWith f = HandlerT $ liftWith $ \run -> liftWith $ \run' -> f (run' . run . unHandlerT)
  restoreT = HandlerT . restoreT . restoreT

instance MonadBaseControl IO m => MonadBaseControl IO (HandlerT m) where
  type StM (HandlerT m) a = ComposeSt HandlerT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance MonadIO m => MonadLogger (HandlerT m) where
    monadLoggerLog loc src lvl msg = do
      f <- HandlerT $ asks envLogFn
      liftIO $ f loc src lvl $ toLogStr msg

runHandlerT
  :: AppEnv
  -> HandlerT m a
  -> m (Either AppError a)
runHandlerT env =
    runExceptT . flip runReaderT env . unHandlerT

hoistApp
  :: Enter h (HandlerT IO) Servant.Handler s
  => AppEnv
  -> h
  -> s
hoistApp env =
    hoistServer $ fromApp env

hoistServer
  :: Enter h m Servant.Handler s
  => (forall a. m a -> Servant.Handler a)
  -> h
  -> s
hoistServer run = enter $ NT run

type HandlerAuthT m = HandlerT (ReaderT UserInfo m)

fromApp
  :: AppEnv
  -> HandlerT IO a
  -> Servant.Handler a
fromApp env action = do
    e <- liftIO $ runHandlerT env action
    either (throwError . toServantError) pure e

toServantError
  :: AppError
  -> ServantErr
toServantError = \case
    ErrUser msg         -> toBS err400 msg
    ErrBug msg          -> toBS err500 msg
    ErrDatabase msg     -> toBS err500 msg
    ErrUnauthorized msg -> toBS err401 msg
    ErrForbidden msg    -> toBS err403 msg
  where
    toBS err str =
      err { errBody = ByteString.Lazy.fromStrict $ Text.encodeUtf8 str }

type IOConstraint m = (MonadBaseControl IO m, MonadIO m)

runQuery
  :: IOConstraint m
  => Esqueleto.SqlPersistT (HandlerT m) a
  -> HandlerT m a
runQuery query = do
  pool <- HandlerT $ asks envDbConnection
  catch (Esqueleto.runSqlPool query pool) $ \(SomeException e) -> do
    let msg = Text.pack $ show e
    $logError "runSqlPool failed."
    $logError $ "Error: " <> msg
    throwError $ ErrDatabase msg

instance IOConstraint m => Db.Read (HandlerT m) where
  get = runQuery . Esqueleto.get
  select = runQuery . Esqueleto.select

instance IOConstraint m => Db.Insert (HandlerT m) where
  insert = runQuery . Esqueleto.insert
  insertSelect = runQuery . Esqueleto.insertSelect

instance IOConstraint m => Db.Delete (HandlerT m) where
  deleteByKey = runQuery . Persist.delete
  delete = runQuery . Esqueleto.delete
  deleteCount = runQuery . Esqueleto.deleteCount

instance IOConstraint m => Db.Update (HandlerT m) where
  update = runQuery . Esqueleto.update
  updateCount = runQuery . Esqueleto.updateCount

instance IOConstraint m => Db.Replace (HandlerT m) where
  replace key record = runQuery $ Esqueleto.replace key record
