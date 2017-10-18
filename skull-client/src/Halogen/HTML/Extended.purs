module Halogen.HTML.Extended
  ( module Halogen.HTML
  , module Halogen.HTML.Extended
  ) where

import Halogen.HTML.Events as Events
import DOM.HTML.Indexed (HTMLbutton)
import Data.Function ((<<<))
import Halogen (Action)
import Halogen.HTML
import Halogen.HTML.Properties (IProp, class_)
import Prelude (Unit, ($), (<>))

cl :: forall i p. String -> IProp ( "class" :: String | p) i
cl = class_ <<< ClassName

cldiv_ :: forall p i.
          String -> Array (HTML p i) -> HTML p i
cldiv_ cls = div [ cl cls ]

clspan_ :: forall p i. String -> Array (HTML p i) -> HTML p i
clspan_ cls = span [ cl cls ]

doNothingForm_ :: forall p i. Array (HTML p i) -> HTML p i
doNothingForm_ = form [ attr (AttrName "onsubmit") "return false" ]

faIcon_ :: forall p i.
           String
        -> HTML p i
faIcon_ icon =
  i [ cl $ "fa fa-" <> icon
    ]
    []

faButton_ :: forall p f.
             String
          -> Action f
          -> HTML p (f Unit)
faButton_ icon query =
  button
    [ Events.onClick $ Events.input_ query
    , class_ $ ClassName "button--pure"
    ]
    [ faIcon_ $ icon <> " fa-fw fa-lg"
    ]

faButton :: forall p i.
            String
         -> Array (IProp HTMLbutton i)
         -> HTML p i
faButton icon props =
  button (
    [ cl "button--pure"
    ] <> props )
    [ faIcon_ (icon <> " fa-fw fa-lg")
    ]
