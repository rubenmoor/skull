{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |

module Handler
  ( HandlerT (..)
  , HandlerProtectedT
  , transform
  , runHandlerT
  ) where

import           Control.Exception.Lifted    (SomeException (..), catch)
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Except        (MonadError)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Logger        (MonadLogger (..))
import           Control.Monad.Reader        (MonadReader (..), ReaderT, asks)
import           Control.Monad.Trans.Class   (MonadTrans (..))
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..),
                                              defaultLiftBaseWith,
                                              defaultLiftWith, defaultRestoreM,
                                              defaultRestoreT)
import qualified Data.ByteString.Lazy        as ByteString.Lazy
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as Text
import qualified Data.Text.Encoding          as Text
import           Database.Gerippe            (SqlPersistT, runSqlPool)
import qualified Database.Gerippe            as Gerippe
import           Diener                      (DienerT (..), LogEnv (logEnv),
                                              logError, runDienerT, throwError)
import           Servant                     ((:~>) (..), Handler,
                                              ServantErr (..), enter, err400,
                                              err401, err403, err500)
import           Servant.Utils.Enter         (Enter)

import           Auth.Types                  (UserInfo)
import qualified Database.Class              as Db
import           Types                       (AppEnv (..), AppError (..), Env)

newtype HandlerT m a = HandlerT { unHandlerT :: DienerT AppError AppEnv m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError AppError, MonadTrans)

deriving instance MonadBase IO m => MonadBase IO (HandlerT m)

instance MonadReader UserInfo m => MonadReader UserInfo (HandlerT m) where
  ask = lift ask

instance MonadTransControl HandlerT where
  type StT HandlerT m = StT (DienerT AppError AppEnv) m
  liftWith = defaultLiftWith HandlerT unHandlerT
  restoreT = defaultRestoreT HandlerT

instance (MonadBase IO m, MonadBaseControl IO m) => MonadBaseControl IO (HandlerT m) where
  type StM (HandlerT m) a = ComposeSt HandlerT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance MonadIO m => MonadLogger (HandlerT m) where
  monadLoggerLog loc src lvl msg = HandlerT $ monadLoggerLog loc src lvl msg

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
  => SqlPersistT (HandlerT m) a
  -> HandlerT m a
runQuery query = do
  pool <- HandlerT $ asks $ envDbConnection . logEnv
  catch (runSqlPool query pool) $ \(SomeException e) -> do
    let msg = Text.pack $ show e
    $logError "runSqlPool failed."
    $logError $ "Error: " <> msg
    throwError $ ErrDatabase msg

instance IOConstraint m => Db.Read (HandlerT m) where
  get = runQuery . Gerippe.get
  getAll = runQuery Gerippe.getAll
  getWhere field value = runQuery $ Gerippe.getWhere field value
  joinMTo1Where' fkField idField field value =
    runQuery $ Gerippe.joinMTo1Where' fkField idField field value

instance IOConstraint m => Db.Insert (HandlerT m) where
  insert = runQuery . Gerippe.insert
  -- TODO: insertBy, insertMany, insertKey, insertRecord, insertSelect ...

instance IOConstraint m => Db.Delete (HandlerT m) where
  delete = runQuery . Gerippe.delete

instance IOConstraint m => Db.Update (HandlerT m) where
  update key = runQuery . Gerippe.replace key
