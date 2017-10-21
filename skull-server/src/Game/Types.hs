{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Game.Types where

import           Data.Aeson          (FromJSON, ToJSON)
import           Data.Text           (Text)
import           Database.Persist.TH (derivePersistField)
import           GHC.Generics        (Generic)
import           Servant.Docs        (ToSample (..), singleSample)

type GameKey = Text
type PlayerKey = Text

sampleGameKey :: GameKey
sampleGameKey = "32rfdaf3"

samplePlayerKey :: PlayerKey
samplePlayerKey = "f40jf920s0"

-- game info

data Info = Info
  { _giKey     :: GameKey
  , _giState   :: State
  , _giPhase   :: Phase
  , _giPlayers :: [Player]
  }
  deriving (Generic, Show, Read, Eq, ToJSON)

sampleInfo :: Info
sampleInfo = Info
  { _giKey = sampleGameKey
  , _giState = sampleState
  , _giPhase = samplePhase
  , _giPlayers = samplePlayers
  }

instance ToSample Info where
  toSamples _ = singleSample sampleInfo

-- state

data State
  = Round Int
  | Finished VictoryInfo
  | Aborted Text
  deriving (Generic, Show, Read, Eq, ToJSON)

sampleState :: State
sampleState = Round 3

data VictoryInfo = VictoryInfo
  { _viWinner :: PlayerKey
  , _viType   :: VictoryType
  , _viRounds :: Int
  } deriving (Generic, Show, Read, Eq, ToJSON)

data VictoryType
  = TwoWins
  | Survivor
  deriving (Generic, Show, Read, Eq, ToJSON)

-- phase

data Phase
  = FirstCard
  | CardOrBet
  | Bet
  | Reveal
  deriving (Generic, ToJSON, Show, Read, Eq)

samplePhase :: Phase
samplePhase = CardOrBet

-- player

data Player = Player
  { _plKey      :: PlayerKey
  , _plNature   :: Nature
  , _plVictory  :: Victory
  , _plHand     :: Hand
  , _plAlive    :: Bool
  , _plStack    :: Stack
  , _plBetState :: BetState
  } deriving (Generic, ToJSON, Show, Read, Eq)

samplePlayer1 :: Player
samplePlayer1 = Player
  { _plKey      = samplePlayerKey
  , _plNature   = Human
  , _plVictory  = None
  , _plHand     = sampleHand1
  , _plAlive    = True
  , _plStack    = Stack []
  , _plBetState = NothingYet
  }

samplePlayers :: [Player]
samplePlayers = [samplePlayer1]

data Hand = Hand
  { _hNumPlains :: Int
  , _hHasSkull  :: Bool
  } deriving (Generic, ToJSON, Show, Read, Eq)

sampleHand1 :: Hand
sampleHand1 = Hand
  { _hNumPlains = 3
  , _hHasSkull = True
  }

data Victory
  = None
  | One
  deriving (Generic, Show, Read, Eq, ToJSON)

newtype Stack = Stack [Card]
  deriving (Generic, Show, Read, Eq, ToJSON)

data Card
  = Skull
  | Plain
  deriving (Generic, Show, Read, Eq, ToJSON, FromJSON)

data Nature
  = Human
  | Bot
  deriving (Generic, Show, Read, Eq, ToJSON)

data BetState
  = NothingYet
  | BetSet Int
  | Fold
  deriving (Generic, Show, Read, Eq, ToJSON)

derivePersistField "Info"
