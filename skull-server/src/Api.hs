{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api
    ( Routes
    , api
    ) where

import           Servant

import qualified Auth.Api   as Auth
import qualified BotKey.Api as BotKey
import qualified Game.Api   as Game

type Routes =
       "auth"   :> Auth.Routes
  :<|> "botkey" :> BotKey.Routes
  :<|> "game"   :> Game.Routes

api :: Proxy Routes
api = Proxy
