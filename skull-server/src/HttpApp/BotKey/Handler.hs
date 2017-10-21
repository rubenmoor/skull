{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module HttpApp.BotKey.Handler where

import           Control.Monad                       (when)
import           Control.Monad.Except                (MonadError, throwError)
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Control.Monad.Reader                (MonadReader, asks)
import           Data.Functor.Extended               (forEach, (<$>))
import           Prelude                             (IO, pure, ($), (==))
import           Servant                             ((:<|>) (..), ServerT)
import           System.Entropy                      (getEntropy)
import           TextShow                            (showt)

import           Auth.Types                          (UserInfo (..))
import qualified Data.ByteString.Base64.URL.Extended as Base64
import qualified Database.Class                      as Db
import           Database.Esqueleto                  (Entity (..),
                                                      InnerJoin (..), from, set,
                                                      val, where_, (&&.), (=.),
                                                      (==.), (^.))
import           Handler                             (HandlerProtectedT)
import qualified HttpApp.BotKey.Api                  as Api
import           HttpApp.BotKey.Api.Types
import           HttpApp.BotKey.Types                (BotKey (..))
import           HttpApp.Model                       (EntityField (..))
import qualified HttpApp.Model                       as Model
import           Types                               (AppError (..))

protected :: ServerT Api.Protected (HandlerProtectedT IO)
protected =
       new
  :<|> all
  :<|> setLabel
  :<|> delete

new
  :: (Db.Insert m, MonadIO m, MonadReader UserInfo m)
  => m BKNewResp
new = do
  let _bkLabel = ""
  botKeySecret <- Base64.encode <$> liftIO (getEntropy 32)
  uId <- asks _uiUserId
  Db.insert_ Model.BotKey
    { botKeyFkUser = uId
    , botKeySecret
    , botKeyLabel  = _bkLabel
    }
  pure BKNewResp
    { _nrespBotKey = BotKey
      { _bkLabel
      , _bkSecret = showt botKeySecret
      }
    }

all
  :: (Db.Read m, MonadReader UserInfo m)
  => m BKAllResp
all = do
  uId <- asks _uiUserId
  bks <- Db.select $ from $ \(u `InnerJoin` bk) -> do
    where_ $ (bk ^. BotKeyFkUser ==. u ^. UserId)
         &&. (u ^. UserId ==. val uId)
    pure bk
  let _arespBotKeys = forEach bks $ \(Entity _ Model.BotKey{..}) -> BotKey
        { _bkLabel = botKeyLabel
        , _bkSecret = showt botKeySecret
        }
  pure BKAllResp{..}

setLabel
  :: (Db.Update m, Db.Read m, MonadReader UserInfo m, MonadError AppError m)
  => BKSetLabelRq
  -> m BKSetLabelResp
setLabel BKSetLabelRq{..} = do
  uId <- asks _uiUserId
  let secret = Base64.fromText _slrqSecret
  n <- Db.updateCount $ \bk -> do
    set bk [ BotKeyLabel =. val _slrqLabel ]
    where_ $ (bk ^. BotKeyFkUser ==. val uId)
         &&. (bk ^. BotKeySecret ==. val secret)
  if n == 0
    then throwError $ ErrDatabase "BotKey not found"
    else pure BKSetLabelResp{ _slrespLabel = _slrqLabel }

delete
  :: (Db.Delete m, MonadReader UserInfo m, MonadError AppError m)
  => BKDeleteRq
  -> m ()
delete BKDeleteRq{..} = do
  uId <- asks _uiUserId
  let secret = Base64.fromText _drqSecret
  n <- Db.deleteCount $ from $ \bk ->
    where_ $ (bk ^. BotKeyFkUser ==. val uId)
         &&. (bk ^. BotKeySecret ==. val secret)
  when (n == 0) $ throwError $ ErrDatabase "BotKey not found"
