module Game.Moves where

import           Control.Lens ((%~), (&), (-~), (.~))

import           Game.Types   (Agent, BetState (..), Card (..), CardFace (..),
                               CardKind (..), aBetState, aHand, aStack,
                               hHasSkull, hNumPlains, stCards)

playPlain :: Agent -> Agent
playPlain agent = agent & aHand . hNumPlains -~ 1
                        & aStack . stCards %~ (:) (Card Plain FaceDown)

playSkull :: Agent -> Agent
playSkull agent = agent & aHand . hHasSkull .~ False
                        & aStack . stCards %~ (:) (Card Skull FaceDown)

placeBet :: Int -> Agent -> Agent
placeBet value = aBetState .~ BetSet value

placeBetFold :: Agent -> Agent
placeBetFold = aBetState .~ Fold
