{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module BotKey.Api where

import           Servant

import           Auth.Types       (SessionProtect)
import           BotKey.Api.Types
import           BotKey.Types     (BotKey)

type Routes =
       SessionProtect :> "botKey" :> "new" :> ReqBody '[JSON] BotKeyNewRequest :> Post '[JSON] ()
  :<|> SessionProtect :> "botKey" :> "all" :> Get '[JSON] [BotKey]
