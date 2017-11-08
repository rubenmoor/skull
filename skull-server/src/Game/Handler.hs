{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RecordWildCards       #-}

module Game.Handler where

import           Prelude                   hiding (all, round)

import           Control.Lens              ((%~), (&), (-~), (.~), (^.))
import           Control.Monad             (when)
import           Control.Monad.Except      (Except, ExceptT, MonadError,
                                            runExcept, runExceptT, throwError)
import           Control.Monad.Reader      (MonadReader, ask)
import           Control.Monad.State       (MonadState, State, get, put,
                                            runState)

import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.Foldable             (for_)
import           Data.List                 (break, sort)
import           Data.List.Ordered         (insertSet)
import           Data.Traversable          (for)
import           Database.Esqueleto        (Entity (..), InnerJoin (..), from,
                                            just, val, where_, (&&.), (==.))
import qualified Database.Esqueleto        as Q ((^.))
import           Servant                   ((:<|>) (..), ServerT)

import           Auth                      (UserInfo, hoistAuthAppBotKey,
                                            uiActiveBotKey, uiUserId)
import qualified Database.Class            as Db
import           Database.Query            (singleCollectSnd)
import           Game                      (gameFromModel, playerFromModel)
import qualified Game.Api                  as Api
import           Game.Api.Types
import           Game.Types
import           Handler.Types             (AppError (..), HandlerAuthT)
import           HttpApp.BotKey.Types      (BotKey, bkSecret)
import           HttpApp.Model             (EntityField (..))
import qualified HttpApp.Model             as Model

handlers :: ServerT Api.Routes (HandlerAuthT IO)
handlers =
          gameJoin
     :<|> playCard

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
  pure $ Result $ undefined

getGame
  :: (Db.Read m, MonadError AppError m)
  => GameKey
  -> m Game
getGame gameKey = do
  ls <- Db.select $ from $ \(g `InnerJoin` pl) -> do
    where_ $ (g Q.^. GameKey ==. val gameKey)
         &&. (pl Q.^. PlayerFkGame ==. g Q.^. GameId)
    pure (g, pl)
  let mGame = do
        (Entity _ game, players) <- singleCollectSnd ls
        pls <- for players $ \(Entity _ player) ->
          playerFromModel gameKey player
        pure $ gameFromModel (sort pls) game
  maybe (throwError $ ErrDatabase "Game not found")
        pure
        mGame

sortCheckPlayers
  :: (Monad m, MonadError AppError m, Db.Read m)
  => Either Model.UserId BotKey
  -> PlayerKey
  -> [Player]
  -> m (Player, [Player])
sortCheckPlayers check key players = do
  case check of
    Left uId -> do
      ls <- Db.select $ from $ \p -> do
        where_ $ (p Q.^. PlayerKey ==. val key)
             &&. (p Q.^. PlayerFkUser ==. just (val uId))
        pure p
      when (null ls) $ throwError $ ErrUnauthorized "No player for given key and user found"
    Right botKey -> do
      ls <- Db.select $ from $ \(p `InnerJoin` bk) -> do
        where_ $ (p Q.^. PlayerKey ==. val key)
             &&. (just (bk Q.^. BotKeyId) ==. p Q.^. PlayerFkBotKey)
             &&. (bk Q.^. BotKeySecret ==. val (botKey ^. bkSecret))
        pure p
      when (null ls) $ throwError $ ErrUnauthorized "No player for given key and botkey"
  case sortPlayers key players of
    Just pls -> pure pls
    Nothing  -> throwError $ ErrDatabase "Player not found in game"

-- sort player by key and rotate active player to front
sortPlayers
  :: PlayerKey
  -> [Player]
  -> Maybe (Player, [Player])
sortPlayers key players =
  let (left, right) = break (\p -> p ^. plKey == key) $ sort players
  in  case right of
        me:others -> Just (me, others ++ left)
        []        -> Nothing

makeMove
  :: (MonadError GameError m, MonadState Game m)
  => (Game -> Player -> Except GameError Player)
  -> Player
  -> m ()
makeMove move player = do
  game <- get
  newPlayer <- either throwError pure $ runExcept $ move game player
  put $ game & gPlayers %~ insertSet newPlayer

withGame
  -- :: (MonadError GameError m, MonadState Game m)
  :: (Db.ReadWrite m, MonadError AppError m, MonadReader UserInfo m)
  => AuthInfo
  -> (Player -> [Player] -> ExceptT GameError (State Game) ())
  -> m (ErrorOr Game)
withGame authInfo action = do
    userInfo <- ask
    let gameKey = authInfo ^. aiGameKey
        playerKey = authInfo ^. aiPlayerKey
        mBotKey = userInfo ^. uiActiveBotKey
        uId = userInfo ^. uiUserId
        check = maybe (Left uId) Right mBotKey
    game <- getGame gameKey
    (me, others) <- sortCheckPlayers check playerKey $ game ^. gPlayers
    let (eResult, newGame) = flip runState game $ runExceptT $ action me others
    -- TODO: persist game
    pure $ case eResult of
      Left err -> Error err
      Right () -> Result newGame

playCard
  :: (Db.ReadWrite m, MonadError AppError m, MonadReader UserInfo m)
  => PlayCardRq
  -> m (ErrorOr Game)
playCard req =
  withGame (req ^. pcrqAuth) $ \me others -> do
    makeMove (movePlayCardMe $ req ^. pcrqCard) me
    for_ others $ \player ->
      makeMove movePlayCardBot player

    -- withError $ do
    --   let numPlayers = undefined
    --   -- check if bot and player id are in the game
    --   let round = undefined
    --   let hand = undefined
    --   newHand <- case req ^. pcrqCard of
    --     Skull -> playSkullOrError hand
    --     Plain -> playPlainOrError hand
    --   -- poll the db every second until timeout
    --   -- once everyone has played, reply with game state
    --   pure $ undefined
  where
    movePlayCardBot
      :: MonadError GameError m
      => Game
      -> Player
      -> m Player
    movePlayCardBot game player = pure player

    movePlayCardMe
      :: MonadError GameError m
      => Card
      -> Game
      -> Player
      -> m Player
    movePlayCardMe card game player = pure player

    withError action = either Error Result <$> runExceptT action
    playSkullOrError h =
      if h ^. hHasSkull
      then pure $ h & hHasSkull .~ False
      else throwError $ GameError "Illegal play: skull. Your hand doesn't have the skull card."
    playPlainOrError h =
      if h ^. hNumPlains > 0
      then pure $ h & hNumPlains -~ 1
      else throwError $ GameError "Illegal play: plain. Your hand doesn't have a plain card."

--

maybeT
  :: Monad m
  => Maybe a
  -> MaybeT m a
maybeT = MaybeT . pure
