module Game where

import Data.Lens ((^.))
import Data.Semiring ((+))
import Game.Types (Hand, hHasSkull, hNumPlains)

handSize :: Hand -> Int
handSize hand =
  hand ^. hNumPlains + if hand ^. hHasSkull then 1 else 0
