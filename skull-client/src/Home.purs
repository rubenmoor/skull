module Home
  ( home
  ) where

import Data.Maybe (Maybe(..))
import Halogen (Component, ComponentDSL, component)
import Halogen.HTML (HTML)
import Home.Render (render)
import Home.Types (Effects, Input, Message, Query(..), State, initialState)
import Prelude (type (~>), const, pure, bind)
import Ulff (Ulff)

home :: forall eff.
        Component HTML Query Input Message (Ulff eff)
home =
  component
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }

eval :: forall eff.
        Query ~> ComponentDSL State Query Message (Ulff  eff)
eval = case _ of
  HandleInput _ next -> do
    pure next
