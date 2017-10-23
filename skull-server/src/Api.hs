{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api
  ( Routes
  ) where

import           Servant     ((:<|>), (:>), Raw)

import           Auth.Types  (AuthProtect)
import qualified Game.Api    as Game
import qualified HttpApp.Api as HttpApp

type Routes  =
       HttpApp.Routes
  :<|> "game" :> Game.Routes
  :<|> Raw
