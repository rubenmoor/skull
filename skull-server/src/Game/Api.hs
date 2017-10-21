{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Game.Api where

import           Servant

import           Game.Api.Types (ErrorOr, GameJoinRequest, PlayFirstCard)
import           Game.Types     (Info)

type Routes =
       Join
  :<|> FirstCard

type Join = "join" :> ReqBody '[JSON] GameJoinRequest :> Post '[JSON] (ErrorOr Info)
type FirstCard = "play" :> "firstcard" :> ReqBody '[JSON] PlayFirstCard :> Post '[JSON] (ErrorOr Info)
