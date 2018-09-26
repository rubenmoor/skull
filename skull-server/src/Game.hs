{-# LANGUAGE RecordWildCards #-}

module Game
  ( module Game
  , module Game.Types
  ) where

import           Control.Lens  ((^.))
import           Control.Monad (guard)
import           Data.List     (find)

import           Game.Types
import qualified HttpApp.Model as Model

-- play now mode

findHumanPlayer :: [Player] -> Maybe Player
findHumanPlayer = find (\p -> p ^. plKind == HumanPlayNow)

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
      playerAlive = _plAlive
      Agent{..} = _plAgent
      playerHand = _aHand
      playerStack = _aStack
      playerBetState = _aBetState
  pure Model.Player{..}

playerFromModel
  :: GameKey
  -> Model.Player
  -> Maybe Player
playerFromModel _plGameKey Model.Player{..} = do
  let _plKey = playerKey
      _plVictory = playerVictory
      _plAlive = playerAlive
      _aHand = playerHand
      _aStack = playerStack
      _aBetState = playerBetState
      _plAgent = Agent{..}
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
      gameStartPlayerKey = _gStartPlayer ^. plKey
  in  Model.Game{..}

gameFromModel
  :: [Player]
  -> Player
  -> Model.Game
  -> Game
gameFromModel _gPlayers _gStartPlayer Model.Game{..} =
  let _gKey = gameKey
      _gState = gameState
      _gPhase = gamePhase
      _gRound = gameRound
  in  Game{..}
