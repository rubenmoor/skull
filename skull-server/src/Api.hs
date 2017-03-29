{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api
    ( Routes
    , api
    ) where

import           Servant

import           Api.Types

type Routes =
       "user" :> "new" :> ReqBod '[JSON] UserNewRequest :> Get '[JSON] ()
       "game" :> "join" :> ReqBody '[JSON] GameJoinRequest :> Get '[JSON] (ErrorOr GameState)
  :<|> "play" :> "firstcard" :> ReqBody '[JSON] PlayFirstCard :> Get '[JSON] (ErrorOr GameState)


api :: Proxy Routes
api = Proxy
