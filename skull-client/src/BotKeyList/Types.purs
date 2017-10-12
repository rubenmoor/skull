module BotKeyList.Types where

import BotKey.Types as BotKey
import DOM (DOM)
import Data.Const (Const(..))
import Data.Lens (lens)
import Data.Lens.Types (Lens')
import Data.List (List(..))
import Data.Unit (Unit)
import Data.Void (Void)
import Halogen.Component.ChildPath (type (\/), type (<\/>))
import HttpApp.BotKey.Types (BotKey(..))
import Types (MkRequestEffects)

type Effects e =
  MkRequestEffects
    ( dom :: DOM
    | e
    )

type Input = Unit

type State =
  { botKeys :: List BotKey
  }

initial :: Input -> State
initial _ =
  { botKeys: Nil
  }

_botKeys :: Lens' State (List BotKey)
_botKeys = lens _.botKeys (\r bs -> r { botKeys = bs })

data Query a
  = HandleInput Input a
  | Initialize a

type ChildQuery =
       BotKey.Query
  <\/> Const Void

type ChildSlot =
     Int
  \/ Void

type Message = Void
