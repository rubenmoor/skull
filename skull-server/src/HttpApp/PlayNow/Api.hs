{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module HttpApp.PlayNow.Api
  ( module HttpApp.PlayNow.Api.Types
  , Protected
  ) where

import           Servant                   ((:<|>), (:>), Delete, Get, JSON,
                                            Post, ReqBody)

import           HttpApp.PlayNow.Api.Types

type Protected =
       New
  :<|> Active
  :<|> Del

type New = "new" :> ReqBody '[JSON] PNNewRq :> Post '[JSON] PNNewResp
type Active = "active" :> Get '[JSON] PNAllResp
type Del = ReqBody '[JSON] PNDeleteRq :> Delete '[JSON] ()
