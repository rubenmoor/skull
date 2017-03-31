module BotKey.Types where

import           Data.Text (Text)

type Label = Text
type Secret = Text

data BotKey = BotKey
  { bkLabel :: Label }
