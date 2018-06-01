{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Game.Play.Api where

import           Servant             ((:<|>), (:>), JSON, Post, ReqBody)

import           Game.Api.Types      (ErrorOr)
import           Game.Play.Api.Types (PlaceBetRq, PlayCardRq)
import           Game.Types          (Game)

type Routes =
         PlayCard
    :<|> PlaceBet

type PlayCard = "playCard" :> ReqBody '[JSON] PlayCardRq :> Post '[JSON] (ErrorOr Game)
type PlaceBet = "placeBet" :> ReqBody '[JSON] PlaceBetRq :> Post '[JSON] (ErrorOr Game)
