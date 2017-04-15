module Root where

import Root.Types
import ErrorMessage.Types as ErrorMessage
import Halogen.HTML.Events as Events
import Control.Monad.Aff (Aff)
import Control.Monad.State (put)
import Data.Maybe (Maybe(..))
import Halogen (Component, ParentDSL, action, lifecycleParentComponent, parentComponent, query')
import Halogen.Component.ChildPath (cp1)
import Halogen.HTML (HTML)
import Prelude (type (~>), bind, const, pure, unit, ($), (<<<))
import Root.Render (render)
import Types (Env)

root :: forall eff.
        Env
     -> Component HTML Query Input Message (Aff (Effects eff))
root env =
  lifecycleParentComponent
    { initialState: const initial
    , render: render env
    , eval: eval env
    , receiver: const Nothing
    , initializer: Just $ action Initialize
    , finalizer: Nothing
    }

eval :: forall eff.
        Env
     -> Query ~> ParentDSL State Query ChildQuery ChildSlot Message (Aff (Effects eff))
eval env = case _ of
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
