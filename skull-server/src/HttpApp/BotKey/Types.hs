{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module HttpApp.BotKey.Types where

import           Data.Aeson   (ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

type Label = Text
type Secret = Text

data BotKey = BotKey
  { bkLabel :: Label
  } deriving (Generic, ToJSON)
