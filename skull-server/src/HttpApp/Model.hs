{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module HttpApp.Model where

import           Data.Eq                             (Eq (..))
import           Data.Function                       (on)
import           Data.Text                           (Text)
import           Data.Time                           (UTCTime)

import           Database.Persist.TH                 (mkMigrate, mkPersist,
                                                      persistLowerCase, share,
                                                      sqlSettings)

import           Data.ByteString.Base64.URL.Extended (Base64)
import           Game.Types                          (BetState, GState, Hand,
                                                      Phase, Stack, Victory)
import           HttpApp.User.Types                  (Email, PwHash, SessionKey,
                                                      UserName)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  name            UserName
  pwHash          PwHash
  email           Email Maybe
  deriving Eq Ord
Session
  fkUser          UserId
  key             SessionKey
  expiry          UTCTime
BotKey
  fkUser          UserId
  created         UTCTime
  label           Text
  secret          Base64
  USecret secret
Game
  fkUser          UserId
  key             Base64
  state           GState
  phase           Phase
  round           Int
Player
  fkGame          GameId
  fkBotKey        BotKeyId Maybe -- Just => Kind: BotUser
  fkUser          UserId   Maybe -- Just => Kind: HumanPlayNow
  key             Base64         -- Nothing, Nothing => Kind: BotLaplace
  victory         Victory
  hand            Hand
  alive           Bool
  stack           Stack
  betState        BetState
|]

instance Eq Game where
  (==) = (==) `on` gameKey

instance Ord Game where
  compare = compare `on` gameKey
