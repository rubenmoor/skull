module HttpApp.BotKey.Model where

import           Data.Int              (Int64)

import           Database.Schema.Types
import           HttpApp.BotKey.Types
import           HttpApp.User.Model    (UserId)

type BotKeyId = BotKeyId' Int64
type BotKey = BotKey' BotKeyId UserId Label Secret
