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
  { _botKeys :: List BotKey
  , _isLoading :: Boolean
  }

initial :: Input -> State
initial _ =
  { _botKeys: Nil
  , _isLoading: true
  }

botKeys :: Lens' State (List BotKey)
botKeys = lens _._botKeys (\r bs -> r { _botKeys = bs })

isLoading :: Lens' State Boolean
isLoading = lens _._isLoading (\r b -> r { _isLoading = b})

data Query a
  = Initialize a
  | HandleMsg BotKey.Message a
  | CreateNew a

type Slot = Int

type Message = Void
