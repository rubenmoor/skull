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
  , transform
  ) where

import           Control.Monad.Except   (MonadError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader (..), asks)
import qualified Data.ByteString.Lazy   as ByteString.Lazy
import qualified Data.Text.Encoding     as Text
import           Diener                 (DienerT, LogEnv (logEnv), runDienerT,
                                         throwError)
import qualified Opaleye
import           Servant                ((:~>) (..), Handler, ServantErr (..),
                                         enter, err400, err401, err403, err500)
import           Servant.Utils.Enter    (Enter)

import qualified Database.Class         as Db
import           Types                  (AppError (..), Env (..))

newtype HandlerT m a = HandlerT { unHandlerT :: DienerT AppError Env m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError AppError)

transform :: (Enter h (HandlerT IO :~> Handler) s)
          => LogEnv Env -> h -> s
transform env = enter $ Nat $ \action ->
    liftIO (runDienerT env $ unHandlerT action)
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

instance Monad m => MonadReader Env (HandlerT m) where
  ask = logEnv <$> HandlerT ask
  local f action = HandlerT $ local (\le -> le { logEnv = f (logEnv le) }) (unHandlerT action)

instance MonadIO m => Db.Insert (HandlerT m) where
  insertMany table rows f = do
    connection <- asks envDbConnection
    liftIO $ Opaleye.runInsertManyReturning connection table rows f
  insertMany_ table rows = do
    connection <- asks envDbConnection
    () <$ liftIO (Opaleye.runInsertMany connection table rows)

instance MonadIO m => Db.Read (HandlerT m) where
  getByQuery query = do
    connection <- asks envDbConnection
    liftIO $ Opaleye.runQuery connection query

instance MonadIO m => Db.Delete (HandlerT m) where
  deleteWhere table predicate = do
    connection <- asks envDbConnection
    () <$ liftIO (Opaleye.runDelete connection table predicate)

instance MonadIO m => Db.Update (HandlerT m) where
  updateWhere table f predicate = do
    connection <- asks envDbConnection
    () <$ liftIO (Opaleye.runUpdate connection table f predicate)
