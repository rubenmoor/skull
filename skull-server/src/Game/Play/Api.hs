{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Game.Play.Api where

import           Servant             ((:>), JSON, Post, ReqBody)

import           Game.Api.Types      (ErrorOr)
import           Game.Play.Api.Types (PlayCardRq)
import           Game.Types          (Game)

type Routes =
         PlayCard

type PlayCard = "playCard" :> ReqBody '[JSON] PlayCardRq :> Post '[JSON] (ErrorOr Game)
