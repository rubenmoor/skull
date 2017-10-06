module LoggedIn where

import Basil (clearSessionKey)
import Data.Lens ((.=))
import Data.Maybe (Maybe(..))
import Halogen (Component, ParentDSL, action, lifecycleParentComponent)
import Halogen.HTML (HTML)
import HttpApp.User.Api.Types (UserNameResponse(..))
import LoggedIn.Render (render)
import LoggedIn.Types (ChildQuery, ChildSlot, Effects, Input, Message, Query(..), State, _userName, initial)
import Prelude (type (~>), Unit, const, discard, ($), ($>))
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
    , receiver: const Nothing
    , initializer: Just $ action Initialize
    , finalizer: Nothing
    }

eval :: forall eff.
        Query ~> ParentDSL State Query ChildQuery ChildSlot Message (Ulff (Effects eff))
eval = case _ of
  Initialize next -> initialize $> next

initialize :: forall eff. ParentDSL State Query ChildQuery ChildSlot Message (Ulff (Effects eff)) Unit
initialize =
    mkRequest' resetAuth getUserName $ \(UserNameResponse r) ->
      _userName .= r.unrName
  where
    resetAuth _ = do
      clearSessionKey
      gotoLocation $ LocLoggedOut $ LocLoggedOutPublic LocHome
