module Root where

import Halogen.Component.ChildPath
import Root.Types
import ErrorMessage.Types as ErrorMessage
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.State (put)
import Data.Maybe (Maybe(..))
import Halogen (Component, ParentDSL, action, parentComponent, query')
import Halogen.HTML (HTML(..))
import Network.HTTP.Affjax (AJAX)
import Prelude (type (~>), bind, const, pure, unit, ($))
import Root.Render (render)
import Types (Env)

root :: forall eff.
        Env
     -> Component HTML Query Input Message (Aff (avar :: AVAR, console :: CONSOLE, ajax :: AJAX | eff))
root env =
  parentComponent
    { initialState: const initial
    , render: render env
    , eval: eval env
    , receiver: const Nothing
    }

eval :: forall eff.
        Env
     -> Query ~> ParentDSL State Query ChildQuery ChildSlot Message (Aff eff)
eval env = case _ of
  HandleGoto next -> do
    put ViewHome
    pure next
  ShowError msg next -> do
    query' cp1 unit (action $ ErrorMessage.Show msg)
    pure next
