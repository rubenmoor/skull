module Util.HTML where

import Halogen.HTML.Events as Events
import DOM.HTML.Indexed (HTMLbutton)
import Halogen (Action)
import Halogen.HTML (AttrName(..), ClassName(..), HTML, attr, button, div, form, i)
import Halogen.HTML.Properties (IProp(..), class_)
import Prelude (Unit, ($), (<>))

cldiv_ :: forall p i.
          String -> Array (HTML p i) -> HTML p i
cldiv_ cls = div [ class_ $ ClassName cls ]


doNothingForm_ :: forall p i. Array (HTML p i) -> HTML p i
doNothingForm_ = form [ attr (AttrName "onsubmit") "return false" ]

faIcon_ :: forall p i.
           String
        -> HTML p i
faIcon_ icon =
  i [ class_ $ ClassName ("fa fa-" <> icon)
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
    [ class_ $ ClassName "button--pure"
    ] <> props )
    [ faIcon_ (icon <> " fa-fw fa-lg")
    ]
