{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module HttpApp.PlayNow.Api
  ( module HttpApp.PlayNow.Api.Types
  , Protected
  ) where

import           Servant                   ((:<|>), (:>), Get, JSON, Post,
                                            ReqBody)

import           HttpApp.PlayNow.Api.Types

type Protected =
       New
  :<|> All

type New = "new" :> ReqBody '[JSON] PNNewRq :> Post '[JSON] PNNewResp
type All = "all" :> Get '[JSON] PNAllResp
