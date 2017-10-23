module BotKeyList
  ( botKeyList
  ) where

import BotKey.Types as BotKey
import BotKeyList.Render (render)
import BotKeyList.Types (Effects, Input, Message, Query(..), Slot, State, botKeys, initial, isLoading)
import Control.Bind (bind)
import Data.Eq ((==))
import Data.Foldable (for_)
import Data.Function (on, ($))
import Data.Lens (assign, use, view, (%=), (.=), (^.))
import Data.List (List(Cons), delete, findIndex, fromFoldable, updateAt)
import Data.Maybe (Maybe(..))
import Halogen (Component, action, lifecycleParentComponent)
import Halogen.Component (ParentDSL)
import Halogen.HTML (HTML)
import HttpApp.BotKey.Api.Types (arespBotKeys, nrespBotKey)
import HttpApp.BotKey.Types (bkSecret)
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
  HandleMsg (BotKey.MsgDelete bk) next -> do
    botKeys %= delete bk
    pure next
  HandleMsg (BotKey.MsgUpdate newBk) next -> do
    bks <- use botKeys
    let mNewBks = do
          i <- findIndex ((==) `on` view bkSecret $ newBk) bks
          updateAt i newBk bks
    for_ mNewBks $ assign botKeys
    pure next
  CreateNew next -> do
    mkRequest postBotKeyNew $ \resp ->
      botKeys %= Cons (resp ^. nrespBotKey)
    pure next
