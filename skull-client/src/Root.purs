module Root where

import ErrorMessage.Types as ErrorMessage
import Data.Maybe (Maybe(..))
import Halogen (Component, ParentDSL, action, parentComponent, query')
import Halogen.Component.ChildPath (cp1)
import Halogen.HTML (HTML)
import Prelude (type (~>), bind, const, pure, unit, ($))
import Root.Render (render)
import Root.Types (ChildQuery, ChildSlot, Effects, Input, Message, Query(..), State, initial)
import Ulff (Ulff)

root :: forall eff.
        Component HTML Query Input Message (Ulff (Effects eff))
root =
  parentComponent
    { initialState: initial
    , render
    , eval
    , receiver: const Nothing
    }

eval :: forall eff.
        Query ~> ParentDSL State Query ChildQuery ChildSlot Message (Ulff (Effects eff))
eval = case _ of
  -- HandleGoto next -> do
  --   _location .= LocLoggedOut (LocLoggedOutPublic LocHome)
  --   pure next
  ShowError msg next -> do
    query' cp1 unit (action $ ErrorMessage.Show msg)
    pure next
