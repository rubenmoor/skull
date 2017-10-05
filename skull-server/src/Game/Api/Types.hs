{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Game.Api.Types where

import           Control.Arrow (second)
import           Data.Aeson    (FromJSON, ToJSON)
import           Data.Bool     (Bool (..))
import           Data.Functor  (fmap)
import           Data.Monoid   ((<>))
import           Data.Proxy    (Proxy (..))
import           Data.Text     (Text)
import           GHC.Generics  (Generic)
import           Prelude       (($))

import           Servant.Docs  (ToSample (..), singleSample)

import           Game.Types

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
  { gjrGameId :: GameId
  , gjrBotId  :: BotId
  } deriving (Generic, FromJSON, ToJSON)

instance ToSample GameJoinRequest where
  toSamples _ =
    let gjrGameId = sampleGameId
        gjrBotId  = sampleBotId
    in  singleSample GameJoinRequest{..}

-- authentication information for every move

data AuthInfo = AuthInfo
  { aiGameId   :: GameId
  , aiBotId    :: BotId
  , aiPlayerId :: PlayerId
  } deriving (Generic, FromJSON, ToJSON)

sampleAuthInfo :: AuthInfo
sampleAuthInfo = AuthInfo
  { aiGameId   = sampleGameId
  , aiBotId    = sampleBotId
  , aiPlayerId = samplePlayerId
  }

data PlayFirstCard = PlayFirstCard
  { pfcCard :: Card
  , pfcAuth :: AuthInfo
  } deriving (Generic, FromJSON, ToJSON)

instance ToSample PlayFirstCard where
  toSamples _ =
    let pfcCard = Plain
        pfcAuth = sampleAuthInfo
    in  singleSample PlayFirstCard{..}

data GameState = GameState
  { gsPlayerId :: PlayerId
  , gsRound    :: Round
  , gsPhase    :: Phase
  , gsMyStack  :: MyStack
  , gsHand     :: Hand
  , gsStacks   :: Stacks
  , gsBets     :: Bets
  } deriving (Generic, ToJSON)

instance ToSample GameState where
  toSamples _ =
    let gsPlayerId = samplePlayerId
        gsRound    = 3
        gsPhase    = CardOrBet
        gsMyStack  = MyStack [Skull, Plain]
        gsHand     =
          let handNumPlains = 2
              handHasSkull = False
          in  Hand{..}
        gsStacks   = Stacks [1, 1, 2]
        gsBets     = Bets [0, 0, 0]
    in  singleSample GameState{..}
