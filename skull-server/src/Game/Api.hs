{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Game.Api where

import           Servant        ((:<|>), (:>), JSON, Post, ReqBody)

import           Auth.Types     (AuthProtect)
import           Game.Api.Types (ErrorOr, GameJoinRequest, PlayCardRq)
import           Game.Types     (Game)

type Routes =
  AuthProtect :>
    (     Join
     :<|> PlayCard
    )

type Join = "join" :> ReqBody '[JSON] GameJoinRequest :> Post '[JSON] (ErrorOr Game)
type PlayCard = "playCard" :> ReqBody '[JSON] PlayCardRq :> Post '[JSON] (ErrorOr Game)
