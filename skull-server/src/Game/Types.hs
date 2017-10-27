{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Game.Types where

import           Control.Lens.TH                     (makeLenses)
import           Data.Aeson                          (FromJSON, ToJSON)
import           Data.ByteString.Base64.URL.Extended (Base64)
import qualified Data.ByteString.Base64.URL.Extended as Base64
import           Data.Text                           (Text)
import           Database.Persist.TH                 (derivePersistField)
import           GHC.Generics                        (Generic)
import           Servant.Docs                        (ToSample (..),
                                                      singleSample)

type GameKey = Base64
type PlayerKey = Base64

sampleGameKey :: GameKey
sampleGameKey = Base64.fromTextUnsafe "32rfdaf3"

samplePlayerKey :: PlayerKey
samplePlayerKey = Base64.fromTextUnsafe "f40jf920s0"

-- game info

data Game = Game
  { _gKey     :: GameKey
  , _gState   :: GState
  , _gPhase   :: Phase
  , _gPlayers :: [Player]
  }
  deriving (Generic, Show, Read, Eq, ToJSON)


sampleGame :: Game
sampleGame = Game
  { _gKey = sampleGameKey
  , _gState = sampleState
  , _gPhase = samplePhase
  , _gPlayers = samplePlayers
  }

instance ToSample Game where
  toSamples _ = singleSample sampleGame

-- state

data GState
  = Round Int
  | Finished VictoryInfo
  | Aborted Text
  deriving (Generic, Show, Read, Eq, ToJSON)

sampleState :: GState
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
  , _plGameKey  :: GameKey
  , _plKind     :: Kind
  , _plVictory  :: Victory
  , _plHand     :: Hand
  , _plAlive    :: Bool
  , _plStack    :: Stack
  , _plBetState :: BetState
  } deriving (Generic, ToJSON, Show, Read, Eq)

samplePlayer1 :: Player
samplePlayer1 = Player
  { _plKey      = samplePlayerKey
  , _plGameKey  = sampleGameKey
  , _plKind     = HumanPlayNow
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

newtype Stack = Stack
  { _stCards :: [Card]
  } deriving (Generic, Show, Read, Eq, ToJSON)

data Card
  = Skull
  | Plain
  deriving (Generic, Show, Read, Eq, ToJSON, FromJSON)

data Kind
  = HumanPlayNow
  | BotUser
  | BotLaplace
  deriving (Generic, Show, Read, Eq, ToJSON)

data BetState
  = NothingYet
  | BetSet Int
  | Fold
  deriving (Generic, Show, Read, Eq, ToJSON)

derivePersistField "Victory"
derivePersistField "Hand"
derivePersistField "Stack"
derivePersistField "BetState"
derivePersistField "GState"
derivePersistField "Phase"

--

makeLenses ''Player
makeLenses ''Stack
makeLenses ''Hand
