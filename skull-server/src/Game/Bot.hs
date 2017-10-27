module Game.Bot where

import           Control.Lens         ((^.))
import           Control.Monad.Random (MonadRandom, getRandom)
import           Game.Agent           (Agent (..), aHand, aStack)
import           Game.Types           (Card (..), Hand (..), Stack (..),
                                       hHasSkull, hNumPlains, stCards)

playCard
  :: MonadRandom m
  => Agent
  -> m Agent
playCard a =
  if a ^. aHand ^. hHasSkull
  then playRandom a
  else pure $ playPlain a

--

playPlain
  :: Agent
  -> Agent
playPlain a = a
  { _aHand = decrPlainsHand $ a ^. aHand
  , _aStack = putOnStack Plain $ a ^. aStack
  }

playSkull
  :: Agent
  -> Agent
playSkull a = a
  { _aHand = decrSkullsHand $ a ^. aHand
  , _aStack = putOnStack Skull $ a ^. aStack
  }

playRandom
  :: MonadRandom m
  => Agent
  -> m Agent
playRandom a = do
  let prob :: Double
      prob = 1 / (1 + fromIntegral (a ^. aHand ^. hNumPlains))
  r <- getRandom
  pure $ if r < prob
    then playSkull a
    else playPlain a

decrPlainsHand
  :: Hand
  -> Hand
decrPlainsHand h =
  h { _hNumPlains = h ^. hNumPlains - 1 }

decrSkullsHand
  :: Hand
  -> Hand
decrSkullsHand h = h { _hHasSkull = False }

putOnStack
  :: Card
  -> Stack
  -> Stack
putOnStack c s =
  s { _stCards = c : s ^. stCards }
