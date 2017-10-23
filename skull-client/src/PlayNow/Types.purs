module PlayNow.Types where

import DOM (DOM)
import Data.Maybe (Maybe(..))
import Data.Unit (Unit)
import Data.Void (Void)
import Game.Types (Info)
import HttpApp.BotKey.Types (BotKey)
import Types (MkRequestEffects)

type Effects e =
  MkRequestEffects
    ( dom :: DOM
    | e
    )

type Input = Unit

type State = Maybe Info

type HumanGame =
  { _hgInfo :: Info
  , _hgBotKey :: BotKey
  }

initial :: Input -> State
initial _ = Nothing

data Query a
  = Initialize a
  | NewGame Int a
  | AbortGame a

type Slot = Int

type Message = Void
