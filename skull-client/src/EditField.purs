module EditField
  ( editField
  ) where

import Halogen.HTML.Events as Events
import Control.Applicative (pure)
import Data.Function (($))
import Data.Lens (use, (.=))
import Data.NaturalTransformation (type (~>))
import EditField.Render (render)
import EditField.Types (Input, Message(..), Query(CancelEditLabel, SubmitLabel, SetLabel, StartEditLabel, HandleInput), State, _editing, _label, _newLabel, initial)
import Halogen (Component, component, raise)
import Halogen.Component (ComponentDSL)
import Halogen.HTML (HTML)
import Prelude (discard, bind)
import Ulff (Ulff)

editField
  :: forall eff.
     Component HTML Query Input Message (Ulff (eff))
editField =
  component
    { initialState: initial
    , render: render
    , eval: eval
    , receiver: Events.input HandleInput
    }

eval
  :: forall eff.
     Query ~> ComponentDSL State Query Message (Ulff (eff))
eval = case _ of
  HandleInput str next -> do
    _label .= str
    pure next
  StartEditLabel next -> do
    _editing .= true
    label <- use _label
    _newLabel .= label
    pure next
  SetLabel str next -> do
    _newLabel .= str
    pure next
  SubmitLabel next -> do
    new <- use _newLabel
    raise $ NewLabel new
    _editing .= false
    pure next
  CancelEditLabel next -> do
    _editing .= false
    _newLabel .= ""
    pure next
