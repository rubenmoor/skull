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
import           Data.Function                       (on)
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
  { _gKey         :: GameKey
  , _gState       :: GState
  , _gPhase       :: Phase
  , _gRound       :: Int
  , _gPlayers     :: [Player]
  , _gStartPlayer :: Player
  }
  deriving (Generic, Show, Read, Eq, ToJSON)


sampleGame :: Game
sampleGame = Game
  { _gKey = sampleGameKey
  , _gState = sampleState
  , _gPhase = samplePhase
  , _gRound = 0
  , _gPlayers = samplePlayers
  , _gStartPlayer = samplePlayer1
  }

instance ToSample Game where
  toSamples _ = singleSample sampleGame

-- state

data GState
  = Active
  | Finished VictoryInfo
  | Aborted Text
  deriving (Generic, Show, Read, Eq, ToJSON)

sampleState :: GState
sampleState = Active

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

type Bid = Int

data Phase
  = FirstCard
  | CardOrBet
  | Bet Int
  | Reveal Bid
  | RoundFinished
  deriving (Generic, ToJSON, Show, Read, Eq)

samplePhase :: Phase
samplePhase = CardOrBet

-- player

data Agent = Agent
  { _aHand      :: Hand
  , _aStack     :: Stack
  , _aBetState  :: BetState
  , _aHandLimit :: Int
  } deriving (Generic, ToJSON, Show, Read)

sampleAgent1 :: Agent
sampleAgent1 = Agent
  { _aHand = sampleHand1
  , _aStack = Stack []
  , _aBetState = NothingYet
  , _aHandLimit = 4
  }

data Player = Player
  { _plKey     :: PlayerKey
  , _plGameKey :: GameKey
  , _plKind    :: Kind
  , _plVictory :: Victory
  , _plAlive   :: Bool
  , _plAgent   :: Agent
  } deriving (Generic, ToJSON, Show, Read)

instance Eq Player where
  (==) = (==) `on` _plKey

instance Ord Player where
  compare = compare `on` _plKey

samplePlayer1 :: Player
samplePlayer1 = Player
  { _plKey      = samplePlayerKey
  , _plGameKey  = sampleGameKey
  , _plKind     = HumanPlayNow
  , _plVictory  = None
  , _plAlive    = True
  , _plAgent    = sampleAgent1
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

startHand :: Hand
startHand = Hand
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

data Card = Card
  { _cardKind :: CardKind
  , _cardFace :: CardFace
  }
  deriving (Generic, Show, Read, Eq, ToJSON, FromJSON)

data CardKind
  = Skull
  | Plain
  deriving (Generic, Show, Read, Eq, ToJSON, FromJSON)

data CardFace
  = FaceUp
  | FaceDown
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

type RandFunc m = m Double

derivePersistField "Victory"
derivePersistField "Hand"
derivePersistField "Stack"
derivePersistField "BetState"
derivePersistField "GState"
derivePersistField "Phase"

--

makeLenses ''Game
makeLenses ''Agent
makeLenses ''Player
makeLenses ''Stack
makeLenses ''Hand
