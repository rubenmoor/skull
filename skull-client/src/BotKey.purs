module BotKey
  ( botKey
  ) where

import Halogen.HTML.Events as Events
import BotKey.Render (render)
import BotKey.Types (Effects, Input, Message, Query(..), State, _botKey, _editing, _newLabel, initial)
import Control.Applicative (pure)
import Data.Lens ((.=))
import Data.NaturalTransformation (type (~>))
import Halogen (Component, component)
import Halogen.Component (ComponentDSL)
import Halogen.HTML (HTML)
import Prelude (discard)
import Ulff (Ulff)

botKey
  :: forall eff.
     Component HTML Query Input Message (Ulff (Effects eff))
botKey =
  component
    { initialState: initial
    , render: render
    , eval: eval
    , receiver: Events.input HandleInput
    }

eval
  :: forall eff.
     Query ~> ComponentDSL State Query Message (Ulff (Effects eff))
eval = case _ of
  HandleInput bk next -> do
    _botKey .= bk
    pure next
  StartEditLabel next -> do
    _editing .= true
    pure next
  SetLabel str next -> do
    pure next
  SubmitLabel str next -> do
    pure next
  CancelEditLabel next -> do
    _editing .= false
    _newLabel .= ""
    pure next
