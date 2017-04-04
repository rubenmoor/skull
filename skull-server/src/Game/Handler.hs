{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Handler where

import           Prelude              hiding (round)

import           Control.Monad.Except (runExceptT, throwError)
import           Servant              ((:<|>) (..), ServerT)

import qualified Database.Class       as Db
import qualified Game.Api             as Api
import           Game.Api.Types
import           Game.Types
import           Handler              (HandlerT)

handlers :: ServerT Api.Routes (HandlerT IO)
handlers =
       gameJoin
  :<|> playFirstCard

gameJoin :: (Db.Insert m, Monad m)
         => GameJoinRequest
         -> m (ErrorOr GameState)
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
  pure $ Result $ GameState
    { gsPlayerId = playerId
    , gsRound = 1
    , gsPhase = FirstCard
    , gsMyStack = MyStack []
    , gsHand = Hand { handNumPlains = 3, handHasSkull = True }
    , gsStacks = Stacks $ replicate numPlayers 0
    , gsBets = Bets $ replicate numPlayers 0
    }

playFirstCard :: (Db.Insert m, Monad m)
              => PlayFirstCard
              -> m (ErrorOr GameState)
playFirstCard PlayFirstCard { pfcCard = card, pfcAuth = auth } =
    withError $ do
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
  where
    withError action = either Error Result <$> runExceptT action
    playSkullOrError h@Hand{ handHasSkull = hasSkull} =
      if hasSkull
      then pure $ h { handHasSkull = False }
      else throwError $ GameError "Illegal play: skull. Your hand doesn't have the skull card."
    playPlainOrError h@Hand{ handNumPlains = numPlains } =
      if numPlains > 0
      then pure $ h { handNumPlains = numPlains - 1}
      else throwError $ GameError "Illegal play: plain. Your hand doesn't have a plain card."
