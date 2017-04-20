module Public
  ( public
  ) where

import Data.Lens ((.=))
import Data.Maybe (Maybe(..))
import Halogen (Component, ComponentDSL, component)
import Halogen.HTML (HTML)
import Prelude (type (~>), const, pure, bind)
import Public.Render (render)
import Public.Types (Effects, Input, Message, Query(..), State, _location, initialState)
import Ulff (Ulff)

public :: forall eff.
        Component HTML Query Input Message (Ulff eff)
public =
  component
    { initialState
    , render
    , eval
    , receiver: const Nothing
    }

eval :: forall eff.
        Query ~> ComponentDSL State Query Message (Ulff  eff)
eval = case _ of
  HandleInput loc next -> do
    _location .= loc
    pure next
