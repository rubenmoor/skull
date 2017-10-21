module BotKeyList
  ( botKeyList
  ) where

import BotKey.Types as BotKey
import BotKeyList.Render (render)
import BotKeyList.Types (Effects, Input, Message, Query(..), Slot, State, botKeys, initial, isLoading)
import Data.Function (($))
import Data.Lens ((%=), (.=), (^.))
import Data.List (List(Cons), delete, fromFoldable)
import Data.Maybe (Maybe(..))
import Halogen (Component, action, lifecycleParentComponent)
import Halogen.Component (ParentDSL)
import Halogen.HTML (HTML)
import HttpApp.BotKey.Api.Types (arespBotKeys, nrespBotKey)
import Prelude (type (~>), const, discard, pure)
import ServerAPI (getBotKeyAll, postBotKeyNew)
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
        Query ~> ParentDSL State Query BotKey.Query Slot Message (Ulff  (Effects eff))
eval = case _ of
  Initialize next -> do
    mkRequest getBotKeyAll $ \resp -> do
      isLoading .= false
      botKeys .= fromFoldable (resp ^. arespBotKeys)
    pure next
  CreateNew next -> do
    mkRequest postBotKeyNew $ \resp ->
      botKeys %= Cons (resp ^. nrespBotKey)
    pure next
  Delete (BotKey.MsgDelete bk) next -> do
    botKeys %= delete bk
    pure next
