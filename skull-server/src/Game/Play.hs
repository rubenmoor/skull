{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Game.Play where

import           Prelude                   (Either (..), Int, Maybe (..), flip,
                                            maybe, pure, sum, ($), (.), (==))



import           Control.Lens              ((%=), (^.))
import           Control.Monad             (Monad)
import           Control.Monad.Except      (MonadError, runExceptT, throwError,
                                            when)

import           Control.Monad.Random      (MonadRandom, evalRand, getRandom,
                                            mkStdGen)

import           Control.Monad.Reader      (MonadReader, ask)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.RWS   (execRWST)
import           Data.Functor              (fmap, (<$>))
import           Data.List                 (break, find, length, null, sort)
import           Data.List.Ordered         (insertSet)
import           Data.Monoid               ((<>))
import           Data.Traversable          (for)

import qualified Database.Class            as Db
import           Database.Esqueleto        (Entity (..), InnerJoin (..), from,
                                            just, val, where_, (&&.), (==.))
import           Database.Query            (singleCollectSnd)

import           Auth                      (UserInfo, uiActiveBotKey, uiUserId)
import qualified Database.Esqueleto        as Q ((^.))
import           Handler.Types             (AppError (..))
import           HttpApp.BotKey.Types      (BotKey, bkSecret)
import           HttpApp.Model             (EntityField (..),
                                            gameStartPlayerKey)
import qualified HttpApp.Model             as Model

import           Game                      (gameFromModel, gameToModel,
                                            playerFromModel)
import           Game.Api.Types            (AuthInfo, ErrorOr (..), aiGameKey,
                                            aiPlayerKey)
import           Game.Play.Types           (Seating (..), WithGame, WithPlayer)

import           Game.Types                (Game (..), GameKey, Player,
                                            PlayerKey, aStack, gPlayers,
                                            gStartPlayer, plAgent, plKey,
                                            stCards)

getMaxBetValue
  :: Game
  -> Int
getMaxBetValue game =
    sum $ forEach (game ^. gPlayers) $
      \player -> length $ player ^. plAgent . aStack . stCards
  where
    forEach = flip fmap

withGame
  :: (Db.Read m, Db.Replace m, MonadError AppError m, MonadReader UserInfo m, MonadRandom m)
  => AuthInfo
  -> WithGame ()
  -> m (ErrorOr Game)
withGame authInfo action = do
    stdGen <- mkStdGen <$> getRandom
    userInfo <- ask
    let gameKey = authInfo ^. aiGameKey
        humanKey = authInfo ^. aiPlayerKey
        mBotKey = userInfo ^. uiActiveBotKey
        uId = userInfo ^. uiUserId
        check = maybe (Left uId) Right mBotKey
    (dbKey, game) <- getGame gameKey
    seating <- sortCheckPlayers check humanKey (game ^. gPlayers) (game ^. gStartPlayer)
    let eResult = evalRand (runExceptT $ execRWST action seating game) stdGen
    case eResult of
      Left err -> pure $ Error err
      Right (newGame, _) -> do
        Db.replace dbKey (gameToModel uId newGame)
        pure $ Result newGame

withPlayer
  :: (Player -> WithPlayer Player)
  -> Player
  -> WithGame ()
withPlayer move player = do
  newPlayer <- lift $ move player
  gPlayers %= insertSet newPlayer

-- like withPlayer, but allow an additional result in the
-- 'move' function that gets passed on
withPlayer'
  :: (Player -> WithPlayer (Player, a))
  -> Player
  -> WithGame a
withPlayer' move player = do
  (newPlayer, x) <- lift $ move player
  gPlayers %= insertSet newPlayer
  pure x

getGame
  :: (Db.Read m, MonadError AppError m)
  => GameKey
  -> m (Model.GameId, Game)
getGame gameKey = do
  ls <- Db.select $ from $ \(g `InnerJoin` pl) -> do
    where_ $ (g Q.^. GameKey ==. val gameKey)
         &&. (pl Q.^. PlayerFkGame ==. g Q.^. GameId)
    pure (g, pl)
  let mGame = do
        (Entity key game, players) <- singleCollectSnd ls
        pls <- for players $ \(Entity _ player) ->
          playerFromModel gameKey player
        startPlayer <- find (\pl -> pl ^. plKey == gameStartPlayerKey game) pls
        pure (key, gameFromModel (sort pls) startPlayer game)
  maybe (throwError $ ErrDatabase "Game not found")
        pure
        mGame

sortCheckPlayers
  :: (Monad m, MonadError AppError m, Db.Read m)
  => Either Model.UserId BotKey
  -> PlayerKey
  -> [Player]
  -> Player
  -> m Seating
sortCheckPlayers check humanKey players startPlayer = do
  case check of
    Left uId -> do
      ls <- Db.select $ from $ \p -> do
        where_ $ (p Q.^. PlayerKey ==. val humanKey)
             &&. (p Q.^. PlayerFkUser ==. just (val uId))
        pure p
      when (null ls) $ throwError $ ErrUnauthorized "No player for given key and user found"
    Right botKey -> do
      ls <- Db.select $ from $ \(p `InnerJoin` bk) -> do
        where_ $ (p Q.^. PlayerKey ==. val humanKey)
             &&. (just (bk Q.^. BotKeyId) ==. p Q.^. PlayerFkBotKey)
             &&. (bk Q.^. BotKeySecret ==. val (botKey ^. bkSecret))
        pure p
      when (null ls) $ throwError $ ErrUnauthorized "No player for given key and botKey"
  case sortPlayers players humanKey startPlayer of
    Just pls -> pure pls
    Nothing  -> throwError $ ErrDatabase "Player not found in game"

-- sort player by key and rotate current start player to front
sortPlayers
  :: [Player]
  -> PlayerKey
  -> Player
  -> Maybe Seating
sortPlayers players humanKey startPlayer =
  case right of
    _seatMe:_seatRight -> Just Seating{..}
    []                 -> Nothing
  where
    rotated = let (firstHalf, secondHalf) = break (== startPlayer) $ sort players
              in  firstHalf <> secondHalf
    (_seatLeft, right) = break (\p -> p ^. plKey == humanKey) rotated
