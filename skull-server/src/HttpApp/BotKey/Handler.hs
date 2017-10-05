{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module HttpApp.BotKey.Handler where

import           Control.Monad.Reader     (MonadReader, asks)
import           Servant                  ((:<|>) (..), ServerT)

import           Auth.Types               (UserInfo)
import qualified Database.Class           as Db
import           Handler                  (HandlerProtectedT)
import qualified HttpApp.BotKey.Api       as Api
import           HttpApp.BotKey.Api.Types
import           HttpApp.BotKey.Types     (BotKey)

protected :: ServerT Api.Protected (HandlerProtectedT IO)
protected =
       botKeyNew
  :<|> botKeyAll

botKeyNew :: (Db.Insert m, Monad m)
          => BotKeyNewRequest
          -> m BotKeyNewResponse
botKeyNew _ =
  pure BotKeyNewResponse

botKeyAll :: (Db.Read m, MonadReader UserInfo m)
          => m BotKeyAllResponse
botKeyAll = undefined
