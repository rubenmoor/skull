{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Game.Api where

import           Servant

import           Game.Api.Types

type Routes =
       "join" :> ReqBody '[JSON] GameJoinRequest :> Post '[JSON] (ErrorOr GameState)
  :<|> "play" :> "firstcard" :> ReqBody '[JSON] PlayFirstCard :> Post '[JSON] (ErrorOr GameState)
