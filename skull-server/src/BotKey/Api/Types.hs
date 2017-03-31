{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module BotKey.Api.Types where

import           Data.Aeson      (FromJSON)
import           GHC.Generics    (Generic)

data BotKeyNewRequest = BotKeyNewRequest
  {
  } deriving (Generic, FromJSON)
