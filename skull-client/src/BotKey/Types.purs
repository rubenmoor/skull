module BotKey.Types where

import EditField.Types as EditField
import DOM (DOM)
import Data.Function (id)
import Data.Unit (Unit)
import HttpApp.BotKey.Types (BotKey)
import Types (MkRequestEffects)

type Effects e =
  MkRequestEffects
    ( dom :: DOM
    | e
    )

-- input

type Input = BotKey

-- state

type State = BotKey

initial :: Input -> State
initial = id

-- query

data Query a
  = HandleInput Input a
  | SetLabel EditField.Message a
  | Delete a

-- children

type Slot = Unit

-- output

data Message
  = MsgDelete BotKey
  | MsgUpdate BotKey
