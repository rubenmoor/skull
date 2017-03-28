{-# LANGUAGE FlexibleContexts #-}

module Handler
  ( handlers
  ) where

import           Control.Monad.Except    (MonadError, throwError)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Data.Text               (Text)
import           Data.Time.Clock         (getCurrentTime)
import           Servant                 ((:<|>) (..), ServerT)

import qualified Api
import qualified Database.Postgres.Class as Db
import qualified Database.Schema         as Db
import           Handler.Types           (HandlerT)

import           Api.Types
import           Types

handlers :: ServerT Api.Routes (HandlerT IO)
handlers =
       gameJoin
  :<|> playFirstCard

gameJoin :: Db.Insert m
         => GameJoinRequest
         -> m GameState
gameJoin GameJoinRequest { gjrGameId = gameId, gjrBotId = botId } = do
  -- find game by id or throw error
  let numPlayers = undefined
  -- find all associated players
  -- check if there is an empty spot or throw error
  -- create player id
  let playerId = undefined
  -- save botId and playerId in game
  -- poll the database every second until timeout
  -- once the game is full, reply with game state
  pure $ GameState
    { gsPlayerId = playerId
    , gsRound = 1
    , gsPhase = FirstCard
    , gsMyStack = MyStack []
    , gsHand = Hand { handNumPlains = 3, handHasSkull = True }
    , gsStacks = Stacks $ replicate numPlayers 0
    , gsBets = Bets $ replicate numPlayers 0
    }

playFirstCard :: Db.Insert m
              => PlayFirstCard
              -> m GameState
playFirstCard PlayFirstCard { pfcCard = card, pfcAuth = auth } = do
  let AuthInfo { aiGameId = gameId, aiBotId = botId, aiPlayerId = playerId } = auth
  -- lookup game by id
  let numPlayers = undefined
  -- check if bot and player id are in the game
  let round = undefined
  let hand = undefined
  newHand <- case card of
    Skull -> playSkullOrError hand
    Plain -> playPlainOrError hand
  -- poll the db every second until timeout
  -- once everyone has played, reply with game state
  pure $ GameState
    { gsPlayerId = playerId
    , gsRound = round
    , gsPhase = CardOrBet
    , gsMyStack = MyStack [card]
    , gsHand = newHand
    , gsStacks = Stacks $ replicate numPlayers 1
    , gsBets = Bets $ replicate numPlayers 0
    }
