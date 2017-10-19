{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module HttpApp.BotKey.Handler where

import           Prelude                             (IO, flip, fmap, pure, ($),
                                                      (<$>))

import           Control.Monad.Except                (MonadError, throwError)
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Control.Monad.Reader                (MonadReader, asks)
import           Servant                             ((:<|>) (..), ServerT)
import           System.Entropy                      (getEntropy)
import           TextShow                            (showt)

import           Auth.Types                          (UserInfo (..))
import qualified Data.ByteString.Base64.URL.Extended as Base64
import qualified Database.Class                      as Db
import           Database.Gerippe                    (Entity (..))
import           Handler                             (HandlerProtectedT)
import qualified HttpApp.BotKey.Api                  as Api
import           HttpApp.BotKey.Api.Types
import           HttpApp.BotKey.Types                (BotKey (..), Secret)
import           HttpApp.Model                       (BotKeyId,
                                                      EntityField (..))
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
  ls <- Db.join1ToMWhere' UserId BotKeyFkUser UserId uId
  let _barBotKeys = flip fmap ls $ \(_, Entity _ Model.BotKey{..}) -> BotKey
        { _bkLabel = botKeyLabel
        , _bkSecret = showt botKeySecret
        }
  pure BotKeyAllResponse{..}

setLabel
  :: (Db.Update m, Db.Read m, MonadReader UserInfo m, MonadError AppError m)
  => BotKeySetLabelRequest
  -> m BotKeySetLabelResponse
setLabel BotKeySetLabelRequest{..} = do
  (bKey, botKey) <- getBotKey _bsrSecret
  Db.update bKey botKey { Model.botKeyLabel = _bsrLabel }
  pure BotKeySetLabelResponse{ _bsresLabel = _bsrLabel }

delete
  :: (Db.Read m, Db.Delete m, MonadReader UserInfo m, MonadError AppError m)
  => BotKeyDeleteRequest
  -> m ()
delete BotKeyDeleteRequest{..} = do
  (bKey, _) <- getBotKey _bdrSecret
  Db.delete bKey

--

getBotKey
  :: (Db.Read m, MonadReader UserInfo m, MonadError AppError m)
  => Secret
  -> m (BotKeyId, Model.BotKey)
getBotKey secret = do
  uId <- asks _uiUserId
  ls <- Db.join1ToMWhere2' UserId BotKeyFkUser
                           UserId uId
                           BotKeySecret (Base64.fromText secret)
  case ls of
    []    -> throwError $ ErrDatabase "botkey not found"
    _:_:_ -> throwError $ ErrDatabase "multiple botkeys in database"
    [(_, Entity bKey botKey)] -> pure (bKey, botKey)
