module BotKey
  ( botKey
  ) where

import EditField.Types as EditField
import Halogen.HTML.Events as Events
import BotKey.Render (render)
import BotKey.Types (Effects, Input, Message(..), Query(..), Slot, State, initial)
import Control.Applicative (pure)
import Control.Monad.State (get, put)
import Data.Function ((#), ($))
import Data.Lens ((.~), (^.))
import Data.NaturalTransformation (type (~>))
import Halogen (Component, parentComponent, raise)
import Halogen.Component (ParentDSL)
import Halogen.HTML (HTML)
import HttpApp.BotKey.Api.Types (BKDeleteRq(BKDeleteRq), BKSetLabelRq(BKSetLabelRq), slrespLabel)
import HttpApp.BotKey.Types (bkLabel, bkSecret)
import Prelude (discard, bind)
import ServerAPI (deleteBotKey, postBotKeySetLabel)
import Ulff (Ulff, mkRequest)

botKey
  :: forall eff.
     Component HTML Query Input Message (Ulff (Effects eff))
botKey =
  parentComponent
    { initialState: initial
    , render: render
    , eval: eval
    , receiver: Events.input HandleInput
    }

eval
  :: forall eff.
     Query ~> ParentDSL State Query EditField.Query Slot Message (Ulff (Effects eff))
eval = case _ of
  HandleInput bk next -> do
    put bk
    pure next
  SetLabel (EditField.NewLabel str) next -> do
    bk <- get
    let body = BKSetLabelRq
          { _slrqSecret: bk ^. bkSecret
          , _slrqLabel: str
          }
    mkRequest (postBotKeySetLabel body) $ \resp ->
      raise $ MsgUpdate $ bk # bkLabel .~ resp ^. slrespLabel
    pure next
  Delete next -> do
    bk <- get
    let body = BKDeleteRq
          { _drqSecret: bk ^. bkSecret
          }
    mkRequest (deleteBotKey body) $ \_ ->
      raise $ MsgDelete bk
    pure next
