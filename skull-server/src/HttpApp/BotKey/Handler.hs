{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module HttpApp.BotKey.Handler where

import           Control.Monad.Except     (MonadError, throwError)
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Control.Monad.Reader     (MonadReader, asks)
import           Servant                  ((:<|>) (..), ServerT)
import           System.Entropy           (getEntropy)
import           TextShow                 (showt)

import           Auth.Types               (UserInfo (..))
import qualified Database.Class           as Db
import           Database.Gerippe         (Entity (..))
import           Handler                  (HandlerProtectedT)
import qualified HttpApp.BotKey.Api       as Api
import           HttpApp.BotKey.Api.Types
import           HttpApp.BotKey.Types     (BotKey (..))
import           HttpApp.Model            (EntityField (..))
import qualified HttpApp.Model            as Model
import           Types                    (AppError (..))
import qualified Util.Base64              as Base64

protected :: ServerT Api.Protected (HandlerProtectedT IO)
protected =
       botKeyNew
  :<|> botKeyAll
  :<|> botKeySetLabel

botKeyNew
  :: (Db.Insert m, MonadIO m, MonadReader UserInfo m)
  => m BotKeyNewResponse
botKeyNew = do
  let bkLabel = ""
  botKeySecret <- Base64.encode <$> liftIO (getEntropy 32)
  uId <- asks _uiUserId
  Db.insert_ Model.BotKey
    { botKeyFkUser = uId
    , botKeySecret
    , botKeyLabel  = bkLabel
    }
  pure BotKeyNewResponse
    { bnrBotKey = BotKey
      { bkLabel
      , bkSecret = showt botKeySecret
      }
    }

botKeyAll
  :: (Db.Read m, MonadReader UserInfo m)
  => m BotKeyAllResponse
botKeyAll = do
  uId <- asks _uiUserId
  ls <- Db.join1ToMWhere' UserId BotKeyFkUser UserId uId
  let barBotKeys = flip fmap ls $ \(_, Entity _ Model.BotKey{..}) -> BotKey
        { bkLabel = botKeyLabel
        , bkSecret = showt botKeySecret
        }
  pure BotKeyAllResponse{..}

botKeySetLabel
  :: (Db.Update m, Db.Read m, MonadReader UserInfo m, MonadError AppError m)
  => BotKeySetLabelRequest
  -> m BotKeySetLabelResponse
botKeySetLabel BotKeySetLabelRequest{..} = do
  uId <- asks _uiUserId
  ls <- Db.join1ToMWhere2' UserId BotKeyFkUser
                           UserId uId
                           BotKeySecret (Base64.fromText bsrSecret)
  (bKey, botKey) <- case ls of
    []    -> throwError $ ErrDatabase "botkey not found"
    _:_:_ -> throwError $ ErrDatabase "multiple botkeys in database"
    [(_, Entity bKey botKey)] -> pure (bKey, botKey)
  Db.update bKey botKey { Model.botKeyLabel = bsrLabel }
  pure BotKeySetLabelResponse{ bsresLabel = bsrLabel }
