module BotKey.Types where

import DOM (DOM)
import Data.Lens (lens)
import Data.Lens.Types (Lens')
import Data.Void (Void)
import HttpApp.BotKey.Types (BotKey(..))
import Types (MkRequestEffects)

type Effects e =
  MkRequestEffects
    ( dom :: DOM
    | e
    )

-- input

type Input = BotKey

-- state

type State =
  { botKey :: BotKey
  , editing :: Boolean
  , newLabel :: String
  }

initial :: Input -> State
initial botKey =
  { botKey
  , newLabel: ""
  , editing: false
  }

_botKey :: Lens' State BotKey
_botKey = lens _.botKey (\r bk -> r { botKey = bk })

_editing :: Lens' State Boolean
_editing = lens _.editing (\r b -> r { editing = b })

_newLabel :: Lens' State String
_newLabel = lens _.newLabel (\r str -> r { newLabel = str })

-- query

data Query a
  = HandleInput Input a
  | StartEditLabel a
  | SetLabel String a
  | SubmitLabel String a
  | CancelEditLabel a

-- output

type Message = Void
