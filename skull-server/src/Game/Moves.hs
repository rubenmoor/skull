module Game.Moves where

import           Control.Lens ((%~), (&), (-~), (.~))

import           Game.Types   (Agent, Card (..), aHand, aStack, hHasSkull,
                               hNumPlains, stCards)

playPlain :: Agent -> Agent
playPlain agent = agent & aHand . hNumPlains -~ 1
                        & aStack . stCards %~ (:) Plain

playSkull :: Agent -> Agent
playSkull agent = agent & aHand . hHasSkull .~ False
                        & aStack . stCards %~ (:) Skull
