module Navigation
  ( navigation
  ) where

import Control.Monad (pure)
import Data.Function (const, ($))
import Data.Maybe (Maybe(..))
import Data.NaturalTransformation (type (~>))
import Halogen (Component, component)
import Halogen.Component (ComponentDSL)
import Halogen.HTML (HTML)
import Navigation.Render (render)
import Navigation.Types (Input, Message, Query(..), State, Effects, initial)
import Prelude (discard)
import Router (Location(..), LoggedInLocation(..), PrivateLocation(..), PublicLocation(..), gotoLocation)
import Ulff (Ulff)

navigation
  :: forall eff.
     Component HTML Query Input Message (Ulff (Effects eff))
navigation =
  component
    { initialState: initial
    , render
    , eval
    , receiver: const Nothing
    }

eval
  :: forall eff.
     Query ~> ComponentDSL State Query Message (Ulff (Effects eff))
eval = case _ of
  HandleInput _ next -> pure next
  GotoHome next -> do
    gotoLocation $ LocLoggedIn $ LocLoggedInPublic LocHome
    pure next
  GotoBotKeys next -> do
    gotoLocation $ LocLoggedIn $ LocPrivate LocBotKeys
    pure next
  GotoPlayNow next -> do
    gotoLocation $ LocLoggedIn $ LocPrivate LocPlayNow
    pure next
