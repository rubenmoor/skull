{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Bot where

import           Prelude              (Bool, Double, Int, any, fromIntegral,
                                       length, max, not, otherwise, pure, ($),
                                       (+), (.), (/), (<), (==), (||))

import           Control.Lens         (use, (^.))
import           Control.Monad.Random (MonadRandom, getRandom)

import           Control.Monad.State  (get)
import           Data.Foldable        (for_)

import           Game.Agent           (agentDo, agentDoBot)
import           Game.Moves           (placeBet, placeBetFold, playPlain,
                                       playSkull)
import           Game.Play            (getMaxBetValue, withBot, withPlayer)
import           Game.Play.Types      (WithGame)
import           Game.Types           (Agent, Card (..), Phase (..), Player,
                                       aHand, aStack, gPhase, hHasSkull,
                                       hNumPlains, plAgent, stCards)

botMoves
  :: [Player]
  -> WithGame ()
botMoves players = do
  phase <- use gPhase
  for_ players $ case phase of
    FirstCard ->
      withBot $ agentDoBot playCard
    CardOrBet ->
      playCardOrBet

numCardsHand
  :: Player
  -> Int
numCardsHand player =
  let s = if player ^. plAgent . aHand . hHasSkull
          then 1
          else 0
  in  s + player ^. plAgent . aHand . hNumPlains

numStack
  :: Player
  -> Int
numStack player = length $player ^. plAgent . aStack . stCards

stackHasSkull
  :: Player
  -> Bool
stackHasSkull player =
  any (== Skull) $ player ^. plAgent . aStack . stCards

playCardOrBet
  :: Player
  -> WithGame ()
playCardOrBet player = do
  let playCardProb = case numCardsHand player of
        0 -> 0 :: Double
        1 -> 0.1
        2 -> 0.3
        3 -> 0.6
  dye <- getRandom -- TODO: depend on player position
  if dye < playCardProb
    then
      -- play card
      withBot (agentDoBot playCard) player
    else do
      -- place bet
      let bluffProb = 0.2 :: Double
      dye <- getRandom
      -- either no skull in stack, or bluffing anyway
      if dye < bluffProb || not (stackHasSkull player)
        then do
          dye <- getRandom :: WithGame Double
          -- for less predictability, randomly modify optimal bet
          let modifier | dye < 0.1 = 1
                       | dye < 0.2 = -1
                       | otherwise = 0 -- 80% chance for optimal bet
          game <- get
          -- never met more than allowed
          -- TODO: whitelist players without skull
          --       blacklist players with skull only
          let value = max (numStack player + modifier) (getMaxBetValue game)
          withPlayer (agentDo $ placeBet value) player
        else
          withPlayer (agentDo placeBetFold) player

playCard
  :: MonadRandom m
  => Agent
  -> m Agent
playCard a =
  if a ^. aHand . hHasSkull
  then playRandom a
  else pure $ playPlain a

playRandom
  :: MonadRandom m
  => Agent
  -> m Agent
playRandom a = do
  let prob :: Double
      prob = 1 / (1 + fromIntegral (a ^. aHand . hNumPlains))
  r <- getRandom
  pure $ if r < prob
    then playSkull a
    else playPlain a
