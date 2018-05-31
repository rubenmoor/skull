{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Game.Agent where

import           Control.Lens         ((^.))
import           Control.Monad        (unless)
import           Control.Monad.Except (ExceptT, MonadError, lift, throwError,
                                       withExceptT)



import           Control.Monad.Random (MonadRandom)
import           Data.List            (partition)
import           Data.Text            (Text)

import           Game.Api.Types       (GameError (..))
import           Game.Types           (Agent (..), BetState (..), Card (..),
                                       Player (..), aBetState, aHand, aStack,
                                       hHasSkull, hNumPlains, plAgent, stCards)



agentDo
  :: MonadRandom m
  => Player
  -> (Agent -> m Agent)
  -> ExceptT GameError m Player
agentDo player f = withExceptT GameError $ do
    oldAgent <- maybe (throwError "basic agent inconcistency before move") pure $
      toAgent player
    newAgent <- lift $ f oldAgent
    checkAgentMove oldAgent newAgent
    pure $ player { _plAgent = newAgent }

checkAgent
  :: Agent
  -> Bool
checkAgent a =
  let nPlainsHand = a ^. aHand . hNumPlains
      nSkullHand = if a ^. aHand . hHasSkull then 1 else 0
      (plainsStack, skullsStack) = partition (Plain ==) $ a ^. aStack . stCards
      nPlainsStack = length plainsStack
      nSkullsStack = length skullsStack
      nHand = nPlainsHand + nSkullHand
      nStack = nPlainsStack + nSkullsStack
      nPlains = nPlainsHand + nPlainsStack
      nSkulls = nSkullHand + nSkullsStack
      nTotal = nHand + nStack
  in     nPlainsHand >= 0
      && nHand <= 4
      && nPlainsStack <= 3
      && nSkullsStack <= 1
      && nStack >= 0
      && nStack <= 4
      && nPlains <= 3
      && nSkulls <= 1
      && nTotal <= 4
      && case a ^. aBetState of
           NothingYet -> True
           BetSet n   -> n >= 0
           Fold       -> True

checkAgentMove
  :: MonadError Text m
  => Agent
  -> Agent
  -> m ()
checkAgentMove a1 a2 = do
  unless (a2 ^. aHand . hNumPlains <= a1 ^. aHand . hNumPlains) $
    throwError "number of plain cards in hand increased"
  unless (a1 ^. aHand . hHasSkull || not (a2 ^. aHand . hHasSkull)) $
    throwError "skull card re-appeared in hand"
  unless (length (a2 ^. aStack . stCards) >= length (a1 ^. aStack . stCards)) $
    throwError "number of cards in stack decreased"
  case a1 ^. aBetState of
       NothingYet -> pure ()
       Fold       -> case a2 ^. aBetState of
                       NothingYet -> throwError "bet state went from fold to 'no bet yet'"
                       Fold       -> pure ()
                       BetSet _   -> throwError "bet state went from fold to 'bet'"
       BetSet n   -> case a2 ^. aBetState of
                       NothingYet -> throwError "bet state went from 'bet' to 'no bet yet'"
                       Fold       -> pure ()
                       BetSet m   -> unless (m >= n) $ throwError "bet decreased"

toAgent
  :: Player
  -> Maybe Agent
toAgent player =
  let agent = player ^. plAgent
  in  if checkAgent agent
      then Just agent
      else Nothing
