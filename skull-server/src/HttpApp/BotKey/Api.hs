{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module HttpApp.BotKey.Api
  ( Protected
  , module HttpApp.BotKey.Api.Types
  ) where

import           Servant                  ((:<|>), (:>), Get, JSON, Post,
                                           ReqBody)

import           HttpApp.BotKey.Api.Types
import           HttpApp.BotKey.Types     (BotKey)

type Protected =
       "new" :> ReqBody '[JSON] BotKeyNewRequest :> Post '[JSON] ()
  :<|> "all" :> Get '[JSON] [BotKey]
