module LoggedIn where

import Halogen.HTML.Events as Events
import Basil (clearSessionKey)
import Control.Monad (pure)
import Data.Lens ((.=), (^.))
import Data.Maybe (Maybe(..))
import Halogen (Component, ParentDSL, action, lifecycleParentComponent)
import Halogen.HTML (HTML)
import HttpApp.User.Api.Types (nrespUserName)
import LoggedIn.Render (render)
import LoggedIn.Types (ChildQuery, ChildSlot, Effects, Input, Message, Query(..), State, location, userName, initial)
import Prelude (type (~>), Unit, discard, ($), ($>))
import Router (Location(..), LoggedOutLocation(..), PublicLocation(..), gotoLocation)
import ServerAPI (getUserName)
import Ulff (Ulff, mkRequest')

loggedIn :: forall eff.
            Component HTML Query Input Message (Ulff (Effects eff))
loggedIn =
  lifecycleParentComponent
    { initialState: initial
    , render
    , eval
    , receiver: Events.input HandleInput
    , initializer: Just $ action Initialize
    , finalizer: Nothing
    }

eval :: forall eff.
        Query ~> ParentDSL State Query ChildQuery ChildSlot Message (Ulff (Effects eff))
eval = case _ of
  Initialize next -> initialize $> next
  HandleInput loc next -> do
    location .= loc
    pure next

initialize
  :: forall eff.
     ParentDSL State Query ChildQuery ChildSlot Message (Ulff (Effects eff)) Unit
initialize =
    mkRequest' resetAuth getUserName $ \resp ->
      userName .= resp ^. nrespUserName
  where
    resetAuth _ = do
      clearSessionKey
      gotoLocation $ LocLoggedOut $ LocLoggedOutPublic LocHome
