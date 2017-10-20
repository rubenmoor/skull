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
import           Prelude                             (Functor, IO, flip, fmap,
                                                      pure, ($), (<$>), (==))
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
  => m BotKeyNewResponse
new = do
  let _bkLabel = ""
  botKeySecret <- Base64.encode <$> liftIO (getEntropy 32)
  uId <- asks _uiUserId
  Db.insert_ Model.BotKey
    { botKeyFkUser = uId
    , botKeySecret
    , botKeyLabel  = _bkLabel
    }
  pure BotKeyNewResponse
    { _bnrBotKey = BotKey
      { _bkLabel
      , _bkSecret = showt botKeySecret
      }
    }

all
  :: (Db.Read m, MonadReader UserInfo m)
  => m BotKeyAllResponse
all = do
  uId <- asks _uiUserId
  bks <- Db.select $ from $ \(u `InnerJoin` bk) -> do
    where_ $ (bk ^. BotKeyFkUser ==. u ^. UserId)
         &&. (u ^. UserId ==. val uId)
    pure bk
  let _barBotKeys = forEach bks $ \(Entity _ Model.BotKey{..}) -> BotKey
        { _bkLabel = botKeyLabel
        , _bkSecret = showt botKeySecret
        }
  pure BotKeyAllResponse{..}

setLabel
  :: (Db.Update m, Db.Read m, MonadReader UserInfo m, MonadError AppError m)
  => BotKeySetLabelRequest
  -> m BotKeySetLabelResponse
setLabel BotKeySetLabelRequest{..} = do
  uId <- asks _uiUserId
  let secret = Base64.fromText _bsrSecret
  n <- Db.updateCount $ \bk -> do
    set bk [ BotKeyLabel =. val _bsrLabel ]
    where_ $ (bk ^. BotKeyFkUser ==. val uId)
         &&. (bk ^. BotKeySecret ==. val secret)
  if n == 0
    then throwError $ ErrDatabase "BotKey not found"
    else pure BotKeySetLabelResponse{ _bsresLabel = _bsrLabel }

delete
  :: (Db.Read m, Db.Delete m, MonadReader UserInfo m, MonadError AppError m)
  => BotKeyDeleteRequest
  -> m ()
delete BotKeyDeleteRequest{..} = do
  uId <- asks _uiUserId
  let secret = Base64.fromText _bdrSecret
  n <- Db.deleteCount $ from $ \bk ->
    where_ $ (bk ^. BotKeyFkUser ==. val uId)
         &&. (bk ^. BotKeySecret ==. val secret)
  when (n == 0) $ throwError $ ErrDatabase "BotKey not found"

--

forEach
  :: Functor m
  => m a
  -> (a -> b)
  -> m b
forEach = flip fmap
