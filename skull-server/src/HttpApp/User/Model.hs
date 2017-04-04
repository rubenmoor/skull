{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module HttpApp.User.Model where

import           Data.Int              (Int64)
import           Data.Time.Clock       (UTCTime)

import           Database.Schema.Types
import           HttpApp.User.Types
import           Util.Base64           (Base64)

type UserId = UserId' Int64
type User = User' UserId UserName PwHash (Maybe Email)

type SessionId = SessionId' Int64
type SessionKey = SessionKey' Base64
type Session = Session' SessionId UserId SessionKey UTCTime
