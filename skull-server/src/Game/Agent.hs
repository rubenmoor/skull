{-# LANGUAGE TemplateHaskell #-}

module Game.Agent where

import           Control.Lens         ((^.))
import           Control.Lens.TH      (makeLenses)
import           Control.Monad        (unless)
import           Control.Monad.Random (MonadRandom)
import           Data.List            (partition)

import           Game.Types           (BetState (..), Card (..), Hand (..),
                                       Player (..), Stack, hHasSkull,
                                       hNumPlains, plBetState, plHand, plStack,
                                       stCards)

data Agent = Agent
  { _aHand     :: Hand
  , _aStack    :: Stack
  , _aBetState :: BetState
  }

makeLenses ''Agent

checkAgent
  :: Agent
  -> Bool
checkAgent a =
  let nPlainsHand = a ^. aHand ^. hNumPlains
      nSkullHand = if a ^. aHand ^. hHasSkull then 1 else 0
      (plainsStack, skullsStack) = partition (Plain ==) $ a ^. aStack ^. stCards
      nPlainsStack = length plainsStack
      nSkullsStack = length skullsStack
      nHand = nPlainsHand + nSkullHand
      nStack = nPlainsStack + nSkullsStack
      nPlains = nPlainsHand + nPlainsStack
      nSkulls = nSkullHand + nSkullsStack
      nTotal = nHand + nStack
  in     nPlainsHand >= 0
      && nHand <= 3
      && nPlainsStack <= 3
      && nSkullsStack <= 1
      && nStack >= 1
      && nStack <= 4
      && nPlains <= 3
      && nSkulls <= 1
      && nTotal == 4
      && case a ^. aBetState of
           NothingYet -> True
           BetSet n   -> n >= 0
           Fold       -> True

checkNewPlayer
  :: Player
  -> Player
  -> Bool
checkNewPlayer p1 p2 =
     p2 ^. plHand ^. hNumPlains <= p1 ^. plHand ^. hNumPlains
  && p1 ^. plHand ^. hHasSkull || not (p2 ^. plHand ^. hHasSkull)
  && length (p2 ^. plStack ^. stCards) >= length (p1 ^. plStack ^. stCards)
  && case p1 ^. plBetState of
       NothingYet -> True
       Fold       -> case p2 ^. plBetState of
                       NothingYet -> False
                       Fold       -> True
                       BetSet _   -> False
       BetSet n   -> case p2 ^. plBetState of
                       NothingYet -> False
                       Fold       -> True
                       BetSet m   -> m >= n

agentDo
  :: MonadRandom m
  => Player
  -> (Agent -> m Agent)
  -> m (Maybe Player)
agentDo p f = agentToPlayer p <$> f (toAgent p)

toAgent
  :: Player
  -> Agent
toAgent pl = Agent
  { _aHand = pl ^. plHand
  , _aStack = pl ^. plStack
  , _aBetState = pl ^. plBetState
  }

agentToPlayer
  :: Player
  -> Agent
  -> Maybe Player
agentToPlayer p a = do
  unless (checkAgent a) Nothing
  let newPlayer = p
        { _plHand = a ^. aHand
        , _plStack = a ^. aStack
        , _plBetState = a ^. aBetState
        }
  unless (checkNewPlayer p newPlayer) Nothing
  pure newPlayer
