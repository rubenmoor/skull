{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api
  ( Routes
  ) where

import           Auth.Types  (AuthProtect)
import           Servant     ((:<|>), (:>), Raw)

import qualified Game.Api    as Game
import qualified HttpApp.Api as HttpApp

type Routes  =
       HttpApp.Routes
  :<|> AuthProtect :> "game" :> Game.Routes
  :<|> Raw
