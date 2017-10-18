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

import           Data.Text                           (Text)
import           Data.Time                           (UTCTime)

import           Database.Persist.TH                 (mkMigrate, mkPersist,
                                                      persistLowerCase, share,
                                                      sqlSettings)

import           Data.ByteString.Base64.URL.Extended (Base64)
import           HttpApp.User.Types                  (Email, PwHash, SessionKey,
                                                      UserName)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  name     UserName
  pwHash   PwHash
  email    Email Maybe
  deriving Eq Ord
Session
  fkUser   UserId
  key      SessionKey
  expiry   UTCTime
BotKey
  fkUser   UserId
  label    Text
  secret   Base64
  USecret  secret
|]
