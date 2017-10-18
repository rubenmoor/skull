module BotKeyList.Types where

import BotKey.Types as BotKey
import DOM (DOM)
import Data.Lens (lens)
import Data.Lens.Types (Lens')
import Data.List (List(..))
import Data.Unit (Unit)
import Data.Void (Void)
import HttpApp.BotKey.Types (BotKey)
import Types (MkRequestEffects)

type Effects e =
  MkRequestEffects
    ( dom :: DOM
    | e
    )

type Input = Unit

type State =
  { botKeys :: List BotKey
  , isLoading :: Boolean
  }

initial :: Input -> State
initial _ =
  { botKeys: Nil
  , isLoading: true
  }

_botKeys :: Lens' State (List BotKey)
_botKeys = lens _.botKeys (\r bs -> r { botKeys = bs })

_isLoading :: Lens' State Boolean
_isLoading = lens _.isLoading (\r b -> r { isLoading = b})

data Query a
  = Initialize a
  | CreateNew a
  | Delete BotKey.Message a

type Slot = Int

type Message = Void
