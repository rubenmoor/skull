module Common.DoNothingForm where

import Halogen.HTML (HTML, attr, AttrName(..), form)

doNothingForm_ :: forall p i. Array (HTML p i) -> HTML p i
doNothingForm_ = form [ attr (AttrName "onsubmit") "return false" ]
