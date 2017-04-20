module BotKeys
  ( botKeys
  ) where

import Data.Maybe (Maybe(..))
import Halogen (Component, ComponentDSL, component)
import Halogen.HTML (HTML)
import BotKeys.Render (render)
import BotKeys.Types (Effects, Input, Message, Query(..), State, initialState)
import Prelude (type (~>), const, pure, bind)
import Ulff (Ulff)

botKeys :: forall eff.
        Component HTML Query Input Message (Ulff eff)
botKeys =
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
