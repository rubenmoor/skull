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

playerFromModel
  :: GameKey
  -> Model.Player
  -> Maybe Player
playerFromModel _plGameKey Model.Player{..} = do
  let _plKey = playerKey
      _plVictory = playerVictory
      _plHand = playerHand
      _plAlive = playerAlive
      _plStack = playerStack
      _plBetState = playerBetState
  _plKind <- case (playerFkUser, playerFkBotKey) of
               (Just _, Nothing)  -> Just HumanPlayNow
               (Nothing, Just _)  -> Just BotUser
               (Nothing, Nothing) -> Just BotLaplace
               _                  -> Nothing
  pure Player{..}

gameToModel
  :: Model.UserId
  -> Game
  -> Model.Game
gameToModel gameFkUser Game{..} =
  let gameKey = _gKey
      gameState = _gState
      gamePhase = _gPhase
      gameRound = _gRound
  in  Model.Game{..}

gameFromModel
  :: [Player]
  -> Model.Game
  -> Game
gameFromModel _gPlayers Model.Game{..} =
  let _gKey = gameKey
      _gState = gameState
      _gPhase = gamePhase
      _gRound = gameRound
  in  Game{..}
