{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
-- |

module Handler.Types
  ( HandlerT (..)
  ) where

import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Control.Monad.Reader    (MonadReader (..), asks)
import           Diener                  (DienerT, LogEnv (logEnv))
import qualified Opaleye

import qualified Database.Postgres.Class as Db
import           Types                   (AppError (..), Env (..))

newtype HandlerT m a = HandlerT { unHandlerT :: DienerT AppError Env m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance Monad m => MonadReader Env (HandlerT m) where
  ask = logEnv <$> HandlerT ask
  local f action = HandlerT $ local (\le -> le { logEnv = f (logEnv le) }) (unHandlerT action)

instance MonadIO m => Db.Insert (HandlerT m) where
  insert table rows f = do
    connection <- asks envDbConnection
    liftIO $ Opaleye.runInsertManyReturning connection table rows f
  insert_ table rows = do
    connection <- asks envDbConnection
    _ <- liftIO $ Opaleye.runInsertMany connection table rows
    pure ()

instance MonadIO m => Db.Read (HandlerT m) where
  getAll query = do
    connection <- asks envDbConnection
    liftIO $ Opaleye.runQuery connection query
