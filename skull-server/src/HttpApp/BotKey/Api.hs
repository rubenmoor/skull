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

type New = "new" :> Post '[JSON] BKNewResp
type All = "all" :> Get '[JSON] BKAllResp
type Set = "set" :> "label" :> ReqBody '[JSON] BKSetLabelRq :> Post '[JSON] BKSetLabelResp
type Del = ReqBody '[JSON] BKDeleteRq :> Delete '[JSON] ()
