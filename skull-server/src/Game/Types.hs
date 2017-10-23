{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Game.Types where

import           Control.Lens         ((^.))
import           Control.Lens.TH      (makeLenses)
import           Control.Monad        (unless)
import           Control.Monad.Random (MonadRandom)
import           Data.Aeson           (FromJSON, ToJSON)
import           Data.List            (partition)
import           Data.Text            (Text)
import           Database.Persist.TH  (derivePersistField)
import           GHC.Generics         (Generic)
import           Servant.Docs         (ToSample (..), singleSample)

type GameKey = Text
type PlayerKey = Text

sampleGameKey :: GameKey
sampleGameKey = "32rfdaf3"

samplePlayerKey :: PlayerKey
samplePlayerKey = "f40jf920s0"

-- game info

data Game = Game
  { _gInfo    :: Info
  , _gPlayers :: [Player]
  }

sampleGame :: Game
sampleGame = Game
  { _gInfo = sampleInfo
  , _gPlayers = samplePlayers
  }

instance ToSample Game where
  toSamples _ = singleSample sampleGame

data Info = Info
  { _giKey   :: GameKey
  , _giState :: GState
  , _giPhase :: Phase
  }
  deriving (Generic, Show, Read, Eq, ToJSON)

sampleInfo :: Info
sampleInfo = Info
  { _giKey = sampleGameKey
  , _giState = sampleState
  , _giPhase = samplePhase
  }

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
  , _plKind     = Human
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
  = Human
  | BotLaplace
  deriving (Generic, Show, Read, Eq, ToJSON)

data BetState
  = NothingYet
  | BetSet Int
  | Fold
  deriving (Generic, Show, Read, Eq, ToJSON)

derivePersistField "Info"
derivePersistField "Kind"
derivePersistField "Victory"
derivePersistField "Hand"
derivePersistField "Stack"
derivePersistField "BetState"

--

makeLenses ''Player
makeLenses ''Stack
makeLenses ''Hand

-- agent

data Agent = Agent
  { _aHand     :: Hand
  , _aStack    :: Stack
  , _aBetState :: BetState
  }

makeLenses ''Agent

checkAgent
  :: Agent
  -> Bool
checkAgent a =
  let nPlainsHand = a ^. aHand ^. hNumPlains
      nSkullHand = if a ^. aHand ^. hHasSkull then 1 else 0
      (plainsStack, skullsStack) = partition (Plain ==) $ a ^. aStack ^. stCards
      nPlainsStack = length plainsStack
      nSkullsStack = length skullsStack
      nHand = nPlainsHand + nSkullHand
      nStack = nPlainsStack + nSkullsStack
      nPlains = nPlainsHand + nPlainsStack
      nSkulls = nSkullHand + nSkullsStack
      nTotal = nHand + nStack
  in     nPlainsHand >= 0
      && nHand <= 3
      && nPlainsStack <= 3
      && nSkullsStack <= 1
      && nStack >= 1
      && nStack <= 4
      && nPlains <= 3
      && nSkulls <= 1
      && nTotal == 4
      && case a ^. aBetState of
           NothingYet -> True
           BetSet n   -> n >= 0
           Fold       -> True

checkNewPlayer
  :: Player
  -> Player
  -> Bool
checkNewPlayer p1 p2 =
     p2 ^. plHand ^. hNumPlains <= p1 ^. plHand ^. hNumPlains
  && p1 ^. plHand ^. hHasSkull || not (p2 ^. plHand ^. hHasSkull)
  && length (p2 ^. plStack ^. stCards) >= length (p1 ^. plStack ^. stCards)
  && case p1 ^. plBetState of
       NothingYet -> True
       Fold       -> case p2 ^. plBetState of
                       NothingYet -> False
                       Fold       -> True
                       BetSet _   -> False
       BetSet n   -> case p2 ^. plBetState of
                       NothingYet -> False
                       Fold       -> True
                       BetSet m   -> m >= n

agentDo
  :: MonadRandom m
  => Player
  -> (Agent -> m Agent)
  -> m (Maybe Player)
agentDo p f = agentToPlayer p <$> f (toAgent p)

toAgent
  :: Player
  -> Agent
toAgent pl = Agent
  { _aHand = pl ^. plHand
  , _aStack = pl ^. plStack
  , _aBetState = pl ^. plBetState
  }

agentToPlayer
  :: Player
  -> Agent
  -> Maybe Player
agentToPlayer p a = do
  unless (checkAgent a) Nothing
  let newPlayer = p
        { _plHand = a ^. aHand
        , _plStack = a ^. aStack
        , _plBetState = a ^. aBetState
        }
  unless (checkNewPlayer p newPlayer) Nothing
  pure newPlayer
