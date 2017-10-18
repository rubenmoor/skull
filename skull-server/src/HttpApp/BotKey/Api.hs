{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module HttpApp.BotKey.Api
  ( Protected
  , module HttpApp.BotKey.Api.Types
  ) where

import           Servant                  ((:<|>), (:>), Delete, Get, JSON,
                                           Post, ReqBody)

import           HttpApp.BotKey.Api.Types

type Protected =
       New
  :<|> All
  :<|> Set
  :<|> Del

type New = "new" :> Post '[JSON] BotKeyNewResponse
type All = "all" :> Get '[JSON] BotKeyAllResponse
type Set = "set" :> "label" :> ReqBody '[JSON] BotKeySetLabelRequest :> Post '[JSON] BotKeySetLabelResponse
type Del = ReqBody '[JSON] BotKeyDeleteRequest :> Delete '[JSON] ()
