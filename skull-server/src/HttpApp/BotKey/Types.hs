{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module HttpApp.BotKey.Types where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

type Label = Text
type Secret = Text

data BotKey = BotKey
  { _bkLabel  :: Label
  , _bkSecret :: Secret
  } deriving (Eq, Generic, ToJSON, FromJSON)

sampleBotKey :: BotKey
sampleBotKey = BotKey
  { _bkLabel = "Paul"
  , _bkSecret = "34t90erfdsf90wf"
  }
