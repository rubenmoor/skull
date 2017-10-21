{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Game.Api.Types where

import           Control.Arrow        (second)
import           Data.Aeson           (FromJSON, ToJSON)
import           Data.Functor         (fmap)
import           Data.Monoid          ((<>))
import           Data.Proxy           (Proxy (..))
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import           Prelude              (($))

import           Servant.Docs         (ToSample (..), singleSample)

import           Game.Types
import           HttpApp.BotKey.Types (BotKey, sampleBotKey)

-- TODO: custom json instances
--  * check overloaded record fields

newtype GameError = GameError { unGameError :: Text }
  deriving (Generic, ToJSON)

data ErrorOr a
  = Error { geMsg :: GameError }
  | Result a
  deriving (Generic, ToJSON)

instance ToSample a => ToSample (ErrorOr a) where
  toSamples _ =
    let results = fmap (second Result) $ toSamples (Proxy :: Proxy a)
        geMsg = GameError "Illegal play: skull. Your hand doesn't have the skull card."
        error = singleSample Error{..}
    in  error <> results

data GameJoinRequest = GameJoinRequest
  { _gjrGameKey :: GameKey
  , _gjrBotKey  :: BotKey
  } deriving (Generic, FromJSON, ToJSON)

instance ToSample GameJoinRequest where
  toSamples _ =
    let _gjrGameKey = sampleGameKey
        _gjrBotKey  = sampleBotKey
    in  singleSample GameJoinRequest{..}

-- authentication information for every move

data AuthInfo = AuthInfo
  { _aiGameKey   :: GameKey
  , _aiBotKey    :: BotKey
  , _aiPlayerKey :: PlayerKey
  } deriving (Generic, FromJSON, ToJSON)

sampleAuthInfo :: AuthInfo
sampleAuthInfo = AuthInfo
  { _aiGameKey   = sampleGameKey
  , _aiBotKey    = sampleBotKey
  , _aiPlayerKey = samplePlayerKey
  }

data PlayFirstCard = PlayFirstCard
  { _pfcCard :: Card
  , _pfcAuth :: AuthInfo
  } deriving (Generic, FromJSON, ToJSON)

instance ToSample PlayFirstCard where
  toSamples _ =
    let _pfcCard = Plain
        _pfcAuth = sampleAuthInfo
    in  singleSample PlayFirstCard{..}
