{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module HttpApp.BotKey.Types where

import           Data.Aeson                          (FromJSON, ToJSON)
import           Data.ByteString.Base64.URL.Extended as Base64
import           Data.Text                           (Text)
import           GHC.Generics                        (Generic)

type Label = Text
type Secret = Base64

data BotKey = BotKey
  { _bkLabel  :: Label
  , _bkSecret :: Secret
  } deriving (Eq, Generic, ToJSON, FromJSON)

sampleBotKey :: BotKey
sampleBotKey = BotKey
  { _bkLabel = "Paul"
  , _bkSecret = Base64.fromTextUnsafe "34t90erfdsf90wf"
  }
