module BotKeyList
  ( botKeyList
  ) where

import BotKeyList.Render (render)
import BotKeyList.Types (ChildQuery, ChildSlot, Effects, Input, Message, Query(..), State, _botKeys, initial)
import Data.Function (($))
import Data.Functor (($>))
import Data.Lens ((.=))
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Unit (Unit)
import Halogen (Component, ComponentDSL, action, component, lifecycleParentComponent)
import Halogen.Component (ParentDSL)
import Halogen.HTML (HTML)
import HttpApp.BotKey.Api.Types (BotKeyAllResponse(..))
import Prelude (type (~>), const, pure, bind)
import ServerAPI (getBotKeyAll)
import Ulff (Ulff, mkRequest)

botKeyList :: forall eff.
        Component HTML Query Input Message (Ulff (Effects eff))
botKeyList =
  lifecycleParentComponent
    { initialState: initial
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ action Initialize
    , finalizer: Nothing
    }

eval :: forall eff.
        Query ~> ParentDSL State Query ChildQuery ChildSlot Message (Ulff  (Effects eff))
eval = case _ of
  HandleInput _ next -> do
    pure next
  Initialize next -> initialize $> next

initialize
  :: forall eff.
     ParentDSL State Query ChildQuery ChildSlot Message (Ulff (Effects eff)) Unit
initialize =
  mkRequest getBotKeyAll $ \(BotKeyAllResponse r) ->
    _botKeys .= fromFoldable r.barBotKeys
