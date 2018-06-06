{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Game.Play where

import           Prelude                   (Either (..), Int, Maybe (..),
                                            either, flip, maybe, pure, sum, ($),
                                            (.), (==))

import           Control.Lens              ((%=), (^.))
import           Control.Monad             (Monad)
import           Control.Monad.Except      (Except, MonadError, runExcept,
                                            runExceptT, throwError, when,
                                            withExceptT)
import           Control.Monad.Random      (MonadRandom, evalRand, evalRandT,
                                            getRandom, mkStdGen)
import           Control.Monad.Trans.Class (lift)


import           Control.Monad.Reader      (MonadReader, ask)
import           Control.Monad.Trans.RWS   (execRWST, runRWS, runRWST)
import           Data.Functor              (fmap, (<$>))
import           Data.List                 (break, length, null, sort)
import           Data.List.Ordered         (insertSet)
import           Data.Traversable          (for)

import qualified Database.Class            as Db
import           Database.Esqueleto        (Entity (..), InnerJoin (..), from,
                                            just, val, where_, (&&.), (==.))
import           Database.Query            (singleCollectSnd)

import           Auth                      (UserInfo, uiActiveBotKey, uiUserId)
import qualified Database.Esqueleto        as Q ((^.))
import           Handler.Types             (AppError (..))
import           HttpApp.BotKey.Types      (BotKey, bkSecret)
import           HttpApp.Model             (EntityField (..))
import qualified HttpApp.Model             as Model

import           Game                      (gameFromModel, gameToModel,
                                            playerFromModel)
import           Game.Api.Types            (AuthInfo, ErrorOr (..),
                                            GameError (..), aiGameKey,
                                            aiPlayerKey)
import           Game.Play.Types           (Seating (..), WithBot, WithGame)

import           Game.Types                (Game (..), GameKey, Player,
                                            PlayerKey, aStack, gPlayers,
                                            plAgent, plKey, stCards)

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
        playerKey = authInfo ^. aiPlayerKey
        mBotKey = userInfo ^. uiActiveBotKey
        uId = userInfo ^. uiUserId
        check = maybe (Left uId) Right mBotKey
    (dbKey, game) <- getGame gameKey
    seating <- sortCheckPlayers check playerKey $ game ^. gPlayers
    let eResult = evalRand (runExceptT $ execRWST action seating game) stdGen
    case eResult of
      Left err -> pure $ Error err
      Right (newGame, _) -> do
        Db.replace dbKey (gameToModel uId newGame)
        pure $ Result newGame

withPlayer
  :: (Player -> Except GameError Player)
  -> Player
  -> WithGame ()
withPlayer move player = do
  let ePlayer = runExcept $ move player
  newPlayer <- either throwError pure ePlayer
  gPlayers %= insertSet newPlayer

withBot
  :: (Player -> WithBot Player)
  -> Player
  -> WithGame ()
withBot move player = do
    newPlayer <- lift $ move player
    gPlayers %= insertSet newPlayer

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
        pure (key, gameFromModel (sort pls) game)
  maybe (throwError $ ErrDatabase "Game not found")
        pure
        mGame

sortCheckPlayers
  :: (Monad m, MonadError AppError m, Db.Read m)
  => Either Model.UserId BotKey
  -> PlayerKey
  -> [Player]
  -> m Seating
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
  -> Maybe Seating
sortPlayers key players =
  let (_seatLeft, right) = break (\p -> p ^. plKey == key) $ sort players
  in  case right of
        _seatMe:_seatRight -> Just Seating{..}
        []                 -> Nothing
