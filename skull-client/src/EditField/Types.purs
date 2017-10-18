module EditField.Types where

import DOM (DOM)
import Data.Lens (lens)
import Data.Lens.Types (Lens')
import Types (MkRequestEffects)

type Effects e =
    ( dom :: DOM
    | e
    )

-- input

type Input = String

-- state

type State =
  { label :: String
  , editing :: Boolean
  , newLabel :: String
  }

initial :: Input -> State
initial str =
  { label: str
  , editing: false
  , newLabel: ""
  }

_label :: Lens' State String
_label = lens _.label (\r str -> r { label = str })

_editing :: Lens' State Boolean
_editing = lens _.editing (\r b -> r { editing = b })

_newLabel :: Lens' State String
_newLabel = lens _.newLabel (\r str -> r { newLabel = str })

-- query

data Query a
  = HandleInput Input a
  | StartEditLabel a
  | SetLabel String a
  | SubmitLabel a
  | CancelEditLabel a

-- output

data Message = NewLabel String
