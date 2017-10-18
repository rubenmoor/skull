module BotKeyList
  ( botKeyList
  ) where

import BotKey.Types as BotKey
import BotKeyList.Render (render)
import BotKeyList.Types (Effects, Input, Message, Query(..), Slot, State, _botKeys, _isLoading, initial)
import Data.Function (($))
import Data.Lens (use, (%=), (.=))
import Data.List (delete, fromFoldable, (:))
import Data.Maybe (Maybe(..))
import Halogen (Component, action, lifecycleParentComponent)
import Halogen.Component (ParentDSL)
import Halogen.HTML (HTML)
import HttpApp.BotKey.Api.Types (BotKeyAllResponse(..), BotKeyNewResponse(..))
import Prelude (type (~>), const, pure, bind, discard)
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
    mkRequest getBotKeyAll $ \(BotKeyAllResponse r) -> do
      _isLoading .= false
      _botKeys .= fromFoldable r.barBotKeys
    pure next
  CreateNew next -> do
    mkRequest postBotKeyNew $ \(BotKeyNewResponse r) -> do
      bks <- use _botKeys
      _botKeys .= r.bnrBotKey : bks
    pure next
  Delete (BotKey.MsgDelete bk) next -> do
    _botKeys %= delete bk
    pure next
