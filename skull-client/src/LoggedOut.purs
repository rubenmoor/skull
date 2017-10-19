module LoggedOut
  ( loggedOut
  ) where

import Data.Lens ((.=))
import Halogen (Component, ParentDSL, parentComponent)
import Halogen.HTML (HTML)
import Halogen.HTML.Events as Events
import LoggedOut.Render (render)
import LoggedOut.Types (ChildQuery, ChildSlot, Effects, Input, Message, Query(..), State, location, initialState)
import Prelude (type (~>), discard, pure)
import Ulff (Ulff)

loggedOut :: forall eff.
        Component HTML Query Input Message (Ulff (Effects eff))
loggedOut =
  parentComponent
    { initialState
    , render
    , eval
    , receiver: Events.input HandleInput
    }

eval :: forall eff.
        Query ~> ParentDSL State Query ChildQuery ChildSlot Message (Ulff (Effects eff))
eval = case _ of
  HandleInput loc next -> do
    location .= loc
    pure next
