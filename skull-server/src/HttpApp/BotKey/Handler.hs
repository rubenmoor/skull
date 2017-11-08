{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module HttpApp.BotKey.Handler where

import           Control.Applicative                 (pure)
import           Control.Monad                       (when)
import           Control.Monad.Except                (MonadError, throwError)
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Control.Monad.Reader                (MonadReader, asks)
import           Data.Eq                             ((==))
import           Data.Function                       (flip, on, ($), (.))
import           Data.Functor.Extended               (forEach, (<$>))
import           Data.List                           (sortBy)
import           Data.Ord                            (compare)
import           Data.Time.Clock                     (getCurrentTime)
import           Prelude                             (IO)
import           Servant                             ((:<|>) (..), ServerT)
import           System.Entropy                      (getEntropy)

import           Auth.Types                          (UserInfo (..))
import qualified Data.ByteString.Base64.URL.Extended as Base64
import qualified Database.Class                      as Db
import           Database.Esqueleto                  (Entity (..),
                                                      InnerJoin (..), from, set,
                                                      val, where_, (&&.), (=.),
                                                      (==.), (^.))
import           Handler.Types                       (AppError (..),
                                                      HandlerAuthT)
import qualified HttpApp.BotKey.Api                  as Api
import           HttpApp.BotKey.Api.Types
import           HttpApp.BotKey.Types                (BotKey (..))
import           HttpApp.Model                       (EntityField (..))
import qualified HttpApp.Model                       as Model

protected :: ServerT Api.Protected (HandlerAuthT IO)
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
  now <- liftIO getCurrentTime
  uId <- asks _uiUserId
  Db.insert_ Model.BotKey
    { botKeyFkUser = uId
    , botKeyCreated = now
    , botKeySecret
    , botKeyLabel  = _bkLabel
    }
  pure BKNewResp
    { _nrespBotKey = BotKey
      { _bkLabel
      , _bkSecret = botKeySecret
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
  let bksSorted = sortBy (flip compare `on` Model.botKeyCreated . entityVal) bks
      _arespBotKeys = forEach bksSorted $ \(Entity _ Model.BotKey{..}) -> BotKey
        { _bkLabel = botKeyLabel
        , _bkSecret = botKeySecret
        }
  pure BKAllResp{..}

setLabel
  :: (Db.Update m, Db.Read m, MonadReader UserInfo m, MonadError AppError m)
  => BKSetLabelRq
  -> m BKSetLabelResp
setLabel BKSetLabelRq{..} = do
  uId <- asks _uiUserId
  n <- Db.updateCount $ \bk -> do
    set bk [ BotKeyLabel =. val _slrqLabel ]
    where_ $ (bk ^. BotKeyFkUser ==. val uId)
         &&. (bk ^. BotKeySecret ==. val _slrqSecret)
  if n == 0
    then throwError $ ErrDatabase "BotKey not found"
    else pure BKSetLabelResp{ _slrespLabel = _slrqLabel }

delete
  :: (Db.Delete m, MonadReader UserInfo m, MonadError AppError m)
  => BKDeleteRq
  -> m ()
delete BKDeleteRq{..} = do
  uId <- asks _uiUserId
  n <- Db.deleteCount $ from $ \bk ->
    where_ $ (bk ^. BotKeyFkUser ==. val uId)
         &&. (bk ^. BotKeySecret ==. val _drqSecret)
  when (n == 0) $ throwError $ ErrDatabase "BotKey not found"
