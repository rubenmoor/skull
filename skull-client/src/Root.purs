module Root where

import Root.Types
import ErrorMessage.Types as ErrorMessage
import Control.Monad.State (put)
import Data.Maybe (Maybe(..))
import Halogen (Component, ParentDSL, action, lifecycleParentComponent, query')
import Halogen.Component.ChildPath (cp1)
import Halogen.HTML (HTML)
import Prelude (type (~>), bind, const, pure, unit, ($))
import Root.Render (render)
import Ulff (Ulff)

root :: forall eff.
        Component HTML Query Input Message (Ulff (Effects eff))
root =
  lifecycleParentComponent
    { initialState: const initial
    , render: render
    , eval: eval
    , receiver: const Nothing
    , initializer: Just $ action Initialize
    , finalizer: Nothing
    }

eval :: forall eff.
        Query ~> ParentDSL State Query ChildQuery ChildSlot Message (Ulff (Effects eff))
eval = case _ of
  Initialize next -> do
    -- for_ mSessionKey $ \sessionKey -> do
    pure next
  HandleLogin userName next -> do
    -- todo preserve old route
    put $ LoggedIn
      { liUserName: userName
      , liRealm: LIRealmPublic ViewHome
      }
    pure next
  HandleGoto next -> do
    put $ LoggedOut (LORealmPublic ViewHome)
    pure next
  ShowError msg next -> do
    query' cp1 unit (action $ ErrorMessage.Show msg)
    pure next
