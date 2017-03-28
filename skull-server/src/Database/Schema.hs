{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Database.Schema
  ( UserName
  , PwHash
  , Email
  , Label
  , Secret
  , UserId
  , BotKeyId
  , users
  , botKeys
  ) where

import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text                  (Text)
import           Opaleye

-- technical
type OptionalId = Maybe (Column PGInt4)

type UserName = Text
type PwHash = Text
type Email = Text

newtype UserId' a = UserId a
$(makeAdaptorAndInstance "pUserId" ''UserId')
type UserId = UserId' (Column PGInt4)

data User' a b c d = User
  { userId     :: a
  , userName   :: b
  , userPwHash :: c
  , userEmail  :: d
  }
$(makeAdaptorAndInstance "pUser" ''User')
type User userId = User' userId
                         (Column UserName)
                         (Column PwHash)
                         (Column Email)

users :: Table (User (UserId' OptionalId)) (User UserId)
users = Table "users" (pUser User
  { userId     = pUserId $ UserId $ optional "id"
  , userName   = required "name"
  , userPwHash = required "pwHash"
  , userEmail  = required "email"
  })

type Label = Text
type Secret = Text

newtype BotKeyId' a = BotKeyId a
$(makeAdaptorAndInstance "pBotKeyId" ''BotKeyId')
type BotKeyId = BotKeyId' (Column PGInt4)

data BotKey' a b c d = BotKey
  { botKeyId     :: a
  , botKeyFkUser :: b
  , botKeyLabel  :: c
  , botKeySecret :: d
  }
$(makeAdaptorAndInstance "pBotKey" ''BotKey')
type BotKey botKeyId = BotKey' botKeyId
                               (Column UserId)
                               (Column Label)
                               (Column Secret)

botKeys :: Table (BotKey (BotKeyId' OptionalId)) (BotKey BotKeyId)
botKeys = Table "botKeys" (pBotKey BotKey
  { botKeyId     = pBotKeyId $ BotKeyId $ optional "id"
  , botKeyFkUser = required "fkUser"
  , botKeyLabel  = required "label"
  , botKeySecret = required "secret"
  })
