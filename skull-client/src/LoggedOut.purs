module LoggedOut
  ( loggedOut
  ) where

import Data.Lens ((.=))
import Data.Maybe (Maybe(..))
import Halogen (Component, ParentDSL, parentComponent)
import Halogen.HTML (HTML)
import LoggedOut.Render (render)
import LoggedOut.Types (ChildQuery, ChildSlot, Effects, Input, Message, Query(..), State, _location, initialState)
import Prelude (type (~>), const, pure, bind, discard)
import Ulff (Ulff)

loggedOut :: forall eff.
        Component HTML Query Input Message (Ulff (Effects eff))
loggedOut =
  parentComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }

eval :: forall eff.
        Query ~> ParentDSL State Query ChildQuery ChildSlot Message (Ulff (Effects eff))
eval = case _ of
  HandleInput loc next -> do
    _location .= loc
    pure next
