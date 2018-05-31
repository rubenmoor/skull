{-# LANGUAGE Rank2Types #-}

module Game.Bot where

import           Control.Lens         ((^.))
import           Control.Monad.Random (MonadRandom, getRandom)

import           Game.Moves           (playPlain, playSkull)
import           Game.Types           (Agent, aHand, hHasSkull, hNumPlains)



playCard
  :: MonadRandom m
  => Agent
  -> m Agent
playCard a =
  if a ^. aHand . hHasSkull
  then playRandom a
  else pure $ playPlain a

--

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
