{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Game.Handler where

import           Prelude              hiding (all, round)

import           Control.Monad.Except (runExceptT, throwError)
import           Servant              ((:<|>) (..), ServerT)

import qualified Database.Class       as Db
import qualified Game.Api             as Api
import           Game.Api.Types
import           Game.Types
import           Handler              (HandlerProtectedT)

handlers :: ServerT Api.Routes (HandlerProtectedT IO)
handlers =
       gameJoin
  :<|> playFirstCard

gameJoin :: (Db.Insert m, Monad m)
         => GameJoinRequest
         -> m (ErrorOr Info)
gameJoin GameJoinRequest { .. } = do
  -- find game by id or throw error
  let numPlayers = undefined
  -- find all associated players
  -- check if there is an empty spot or throw error
  -- create player id
  let playerId = undefined
  -- save botId and playerId in game
  -- poll the database every second until timeout
  -- once the game is full, reply with game state
  pure $ Result $ undefined

playFirstCard :: (Db.Insert m, Monad m)
              => PlayFirstCard
              -> m (ErrorOr Info)
playFirstCard PlayFirstCard { .. } =
    withError $ do
      -- lookup game by id
      let numPlayers = undefined
      -- check if bot and player id are in the game
      let round = undefined
      let hand = undefined
      newHand <- case _pfcCard of
        Skull -> playSkullOrError hand
        Plain -> playPlainOrError hand
      -- poll the db every second until timeout
      -- once everyone has played, reply with game state
      pure $ undefined
  where
    withError action = either Error Result <$> runExceptT action
    playSkullOrError h@Hand{ _hHasSkull = hasSkull} =
      if hasSkull
      then pure $ h { _hHasSkull = False }
      else throwError $ GameError "Illegal play: skull. Your hand doesn't have the skull card."
    playPlainOrError h@Hand{ _hNumPlains = numPlains } =
      if numPlains > 0
      then pure $ h { _hNumPlains = numPlains - 1}
      else throwError $ GameError "Illegal play: plain. Your hand doesn't have a plain card."
