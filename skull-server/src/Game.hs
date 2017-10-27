{-# LANGUAGE RecordWildCards #-}

module Game
  ( module Game
  , module Game.Types
  ) where

import           Control.Monad (guard)

import           Game.Types
import qualified HttpApp.Model as Model

-- to model

playerToModel
  :: Model.GameId
  -> Maybe Model.BotKeyId
  -> Maybe Model.UserId
  -> Player
  -> Maybe Model.Player
playerToModel playerFkGame playerFkBotKey playerFkUser Player{..} = do
  guard $ case (_plKind, playerFkBotKey, playerFkUser) of
    (HumanPlayNow, Nothing, Just _) -> True
    (BotUser, Just _, Nothing)      -> True
    (BotLaplace, Nothing, Nothing)  -> True
    _                               -> False
  let playerKey = _plKey
      playerVictory = _plVictory
      playerHand = _plHand
      playerAlive = _plAlive
      playerStack = _plStack
      playerBetState = _plBetState
  pure Model.Player{..}

gameToModel
  :: Model.UserId
  -> Game
  -> Model.Game
gameToModel gameFkUser Game{..} =
  let gameKey = _gKey
      gameState = _gState
      gamePhase = _gPhase
  in  Model.Game{..}

gameFromModel
