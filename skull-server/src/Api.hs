{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api
    ( Routes
    , api
    ) where

import           Servant

import           Api.Types

type Routes =
       "game" :> "join" :> Post '[JSON] GameJoinRequest :> Get '[JSON] GameState
  :<|> "play" :> "firstcard" :> Post '[JSON] PlayFirstCard :> Get '[JSON] GameState


api :: Proxy Routes
api = Proxy
