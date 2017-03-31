module BotKey.Model where

import           Data.Int              (Int64)

import           Auth.Model            (UserId)
import           BotKey.Types
import           Database.Schema.Types

type BotKeyId = BotKeyId' Int64
type BotKey = BotKey' BotKeyId UserId Label Secret
