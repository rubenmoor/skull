{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api
  ( Routes
  , HttpAppRoutes
  , PlayNowRoutes
  ) where

import           Auth.Types    (AuthProtect)
import           Servant       ((:<|>), (:>), Raw)

import qualified Game.Api      as Game
import qualified Game.Play.Api as Game.Play
import qualified HttpApp.Api   as HttpApp

type Routes  =
       HttpAppRoutes
  :<|> AuthProtect :> "game" :> Game.Routes
  :<|> PlayNowRoutes
  :<|> Raw

-- routes for purescript app

type HttpAppRoutes = HttpApp.Routes
type PlayNowRoutes = AuthProtect :> "game" :> Game.Play.Routes
