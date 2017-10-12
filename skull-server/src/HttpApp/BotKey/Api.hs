{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module HttpApp.BotKey.Api
  ( Protected
  , module HttpApp.BotKey.Api.Types
  ) where

import           Servant                  ((:<|>), (:>), Get, JSON, Post,
                                           ReqBody)

import           HttpApp.BotKey.Api.Types

type Protected =
       "new" :> Post '[JSON] BotKeyNewResponse
  :<|> "all" :> Get '[JSON] BotKeyAllResponse
  :<|> "set" :> "label" :> ReqBody '[JSON] BotKeySetLabelRequest :> Post '[JSON] BotKeySetLabelResponse
