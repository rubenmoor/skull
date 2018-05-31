{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Game.Api where

import           Servant        ((:>), JSON, Post, ReqBody)

import           Game.Api.Types (ErrorOr, GameJoinRequest)
import           Game.Types     (Game)

type Routes =
         Join

type Join = "join" :> ReqBody '[JSON] GameJoinRequest :> Post '[JSON] (ErrorOr Game)
