{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Api.Types where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text    (Text)
import           GHC.Generics (Generic)
import           Types

newtype GameError = GameError { unGameError :: Text }
  deriving (Generic, ToJSON)

data ErrorOr a
  = Error { geMsg :: GameError }
  | Result a
  deriving (Generic, ToJSON)

type GameId = Text
type BotId = Text
type PlayerId = Text

data GameJoinRequest = GameJoinRequest
  { gjrGameId :: GameId
  , gjrBotId  :: BotId
  } deriving (Generic, FromJSON)

-- authentication information for every move

data AuthInfo = AuthInfo
  { aiGameId   :: GameId
  , aiBotId    :: BotId
  , aiPlayerId :: PlayerId
  } deriving (Generic, FromJSON)

data PlayFirstCard = PlayFirstCard
  { pfcCard :: Card
  , pfcAuth :: AuthInfo
  } deriving (Generic, FromJSON)

data GameState = GameState
  { gsPlayerId :: PlayerId
  , gsRound    :: Round
  , gsPhase    :: Phase
  , gsMyStack  :: MyStack
  , gsHand     :: Hand
  , gsStacks   :: Stacks
  , gsBets     :: Bets
  } deriving (Generic, ToJSON)
