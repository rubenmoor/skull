{-# LANGUAGE RecordWildCards #-}

module Game.Handler where



import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Servant                   (ServerT)


import qualified Database.Class            as Db
import qualified Game.Api                  as Api
import           Game.Api.Types
import           Game.Types
import           Handler.Types             (HandlerAuthT)

handlers :: ServerT Api.Routes (HandlerAuthT IO)
handlers =
          gameJoin

gameJoin
  :: (Db.Insert m, Monad m)
  => GameJoinRequest
  -> m (ErrorOr Game)
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
  pure $ Result undefined

--

maybeT
  :: Monad m
  => Maybe a
  -> MaybeT m a
maybeT = MaybeT . pure
