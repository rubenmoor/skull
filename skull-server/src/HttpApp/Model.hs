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

import           Data.Time            (UTCTime)

import           Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase,
                                       share, sqlSettings)

import           HttpApp.BotKey.Types (Label, Secret)
import           HttpApp.User.Types   (Email, PwHash, SessionKey, UserName)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  name     UserName
  pwHash   PwHash
  email    Email Maybe
Session
  fkUser   UserId
  key      SessionKey
  expiry   UTCTime
BotKey
  fkUser   UserId
  label    Label
  secret   Secret
|]
