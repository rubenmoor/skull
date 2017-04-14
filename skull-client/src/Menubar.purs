module Menubar
  ( menubar
  ) where

import Auth.Types (AuthToken(..))
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR, putVar)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen (Component, ComponentDSL, component, liftAff)
import Halogen.HTML (HTML(..))
import Menubar.Render (render)
import Menubar.Types (Input, Message, Query(..), State, initial)
import Network.HTTP.Affjax (AJAX)
import Prelude (type (~>), bind, const, flip, pure, ($))
import Servant.PureScript.Affjax (errorToString)
import ServerAPI (getUserLogout)
import Types (Env)

menubar :: forall eff.
           Env
        -> Component HTML Query Input Message (Aff (avar :: AVAR, console :: CONSOLE, ajax :: AJAX | eff))
menubar env =
  component
    { initialState: initial
    , render: render env
    , eval: eval env
    , receiver: const Nothing
    }

eval :: forall eff.
        Env
     -> Query ~> ComponentDSL State Query Message (Aff (avar :: AVAR, console :: CONSOLE, ajax :: AJAX | eff))
eval env = case _ of
  Logout next -> do
    eResult <- runExceptT $ flip runReaderT env.apiSettings $ getUserLogout $ AuthToken ""
    liftAff $ case eResult of
      Left err    -> do log $ errorToString err
                        putVar env.ajaxError $ { title: "Ajax Error", details: errorToString err}
      Right unit  -> log "log out, tbd."
    pure next
