module Root where

import ErrorMessage.Types as ErrorMessage
import Data.Lens ((.=))
import Data.Maybe (Maybe(..))
import Halogen (Component, ParentDSL, action, parentComponent, query')
import Halogen.Component.ChildPath (cp1)
import Halogen.HTML (HTML)
import Prelude (type (~>), bind, const, pure, unit, ($), discard)
import Root.Render (render)
import Root.Types (ChildQuery, ChildSlot, Effects, Input, Message, Query(..), State, location, initial)
import Types (UrlRoot)
import Ulff (Ulff)

root
  :: forall eff.
     UrlRoot
  -> Component HTML Query Input Message (Ulff (Effects eff))
root urlRoot =
  parentComponent
    { initialState: initial
    , render: render urlRoot
    , eval
    , receiver: const Nothing
    }

eval :: forall eff.
        Query ~> ParentDSL State Query ChildQuery ChildSlot Message (Ulff (Effects eff))
eval = case _ of
  GotoLocation loc next -> do
    location .= loc
    pure next
  ShowError msg next -> do
    _ <- query' cp1 unit (action $ ErrorMessage.Show msg)
    pure next
