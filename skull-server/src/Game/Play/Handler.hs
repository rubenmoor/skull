{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Game.Play.Handler where

import           Control.Lens         ((%~), (&), (^.))
import           Control.Monad        (when)
import           Control.Monad.Except (ExceptT, MonadError, runExceptT,
                                       throwError)

import           Control.Monad.Random (MonadRandom, Rand, StdGen, evalRand,
                                       getRandom, mkStdGen)
import           Control.Monad.Reader (MonadReader, ask)
import           Control.Monad.State  (MonadState, State, get, put, runState)

import           Data.Foldable        (for_)
import           Data.List            (break, sort)
import           Data.List.Ordered    (insertSet)
import           Data.Traversable     (for)
import           Database.Esqueleto   (Entity (..), InnerJoin (..), from, just,
                                       val, where_, (&&.), (==.))
import qualified Database.Esqueleto   as Q ((^.))
import           Database.Query       (singleCollectSnd)

import           Servant              (ServerT)

import           Auth                 (UserInfo, uiActiveBotKey, uiUserId)
import qualified Database.Class       as Db
import           Game                 (gameFromModel, playerFromModel)
import           Handler.Types        (AppError (..), HandlerAuthT)

import           Game.Agent           (agentDo)
import           Game.Api.Types       (AuthInfo, ErrorOr (..), GameError (..),
                                       aiGameKey, aiPlayerKey)
import qualified Game.Bot             as Bot
import           Game.Moves           (playPlain, playSkull)
import qualified Game.Play.Api        as Api
import           Game.Play.Api.Types
import           Game.Types
import           HttpApp.BotKey.Types (BotKey, bkSecret)
import           HttpApp.Model        (EntityField (..))
import qualified HttpApp.Model        as Model

handlers :: ServerT Api.Routes (HandlerAuthT IO)
handlers =
          playCard

playCard
  :: (Db.ReadWrite m, MonadError AppError m, MonadReader UserInfo m, MonadRandom m)
  => PlayCardRq
  -> m (ErrorOr Game)
playCard req = do
  stdGen <- mkStdGen <$> getRandom
  withGame (req ^. pcrqAuth) $ \me others -> do
    makeMove stdGen (movePlayCardMe $ req ^. pcrqCard) me
    for_ others $ \player ->
      makeMove stdGen movePlayCardBot player
    -- TODO: game state

  where
    movePlayCardBot
      :: MonadRandom m
      => Game
      -> Player
      -> ExceptT GameError m Player
    movePlayCardBot _ player =
      agentDo player Bot.playCard

    movePlayCardMe
      :: MonadRandom m
      => Card
      -> Game
      -> Player
      -> ExceptT GameError m Player
    movePlayCardMe card _ player =
      agentDo player $ \agent -> pure $
        case card of
          Plain -> playPlain agent
          Skull -> playSkull agent

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

makeMove
  :: (MonadError GameError m, MonadState Game m)
  => StdGen
  -> (Game -> Player -> ExceptT GameError (Rand StdGen) Player)
  -> Player
  -> m ()
makeMove stdGen move player = do
  game <- get
  let ePlayer = flip evalRand stdGen $ runExceptT $ move game player
  newPlayer <- either throwError pure ePlayer
  put $ game & gPlayers %~ insertSet newPlayer

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
