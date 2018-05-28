{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module HttpApp.PlayNow.Handler where

import           Control.Lens                        (view)
import           Control.Monad                       (when)
import           Control.Monad.Except                (MonadError, throwError)
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Control.Monad.Random                (evalRand, getStdGen)
import           Control.Monad.Reader                (MonadReader, asks)
import           Data.Foldable                       (for_)
import           Data.List                           (sort)
import           Data.Traversable                    (for)
import           Database.Esqueleto                  (Entity (..),
                                                      InnerJoin (..), from, set,
                                                      val, where_, (&&.), (=.),
                                                      (==.), (^.))
import           Servant                             ((:<|>) (..), ServerT)
import           System.Entropy                      (getEntropy)

import           Auth.Types                          (UserInfo (..), uiUserId)
import qualified Data.ByteString.Base64.URL.Extended as Base64
import           Data.Functor.Extended               ((<$>))
import qualified Database.Class                      as Db
import           Database.Query                      (singleCollectSnd)
import           Game
import           Game.Agent                          (agentDo)
import           Game.Bot                            (playCard)
import           Handler.Types                       (AppError (..),
                                                      HandlerAuthT)
import           HttpApp.Model                       (EntityField (..))
import qualified HttpApp.Model                       as Model
import qualified HttpApp.PlayNow.Api                 as Api
import           HttpApp.PlayNow.Api.Types

protected :: ServerT Api.Protected (HandlerAuthT IO)
protected =
       new
  :<|> active
  :<|> del

new
  :: (Monad m, MonadIO m, MonadError AppError m, MonadReader UserInfo m, Db.Insert m)
  => PNNewRq
  -> m PNNewResp
new PNNewRq{..} = do
  when (_nrqNumPlayers < 2 || _nrqNumPlayers > 4) $
    throwError $ ErrUser "The allowed number of players is between 2 and 4"

  -- game key

  _gKey <- Base64.encode <$> liftIO (getEntropy 32)

  -- players

  humanKey <- Base64.encode <$> liftIO (getEntropy 32)
  let humanPlayer = Player
        { _plKey = humanKey
        , _plGameKey = _gKey
        , _plKind = HumanPlayNow
        , _plVictory = None
        , _plHand = startHand
        , _plAlive = True
        , _plStack = Stack []
        , _plBetState = NothingYet
        }
  botPlayers <- for [(1 :: Int)..(_nrqNumPlayers - 1)] $ \_ -> do
    key <- Base64.encode <$> liftIO (getEntropy 32)
    let p = humanPlayer
          { _plKey = key
          , _plKind = BotLaplace
          }
    stdGen <- liftIO getStdGen
    case evalRand (agentDo p playCard) stdGen of
      Just player -> pure player
      Nothing     -> throwError $ ErrBug "illegal move"
  let allPlayers = sort $ humanPlayer : botPlayers

  -- game

  uId <- asks $ view uiUserId
  let _gState = Active
      _gPhase = FirstCard
      _gRound = 0
      _gPlayers = allPlayers
      _nrespGame = Game{..}

  -- persist

  gameId <- Db.insert $ gameToModel uId _nrespGame
  for_ allPlayers $ \player ->
    let mUserId = case view plKind player  of
                    HumanPlayNow -> Just uId
                    _            -> Nothing
    in  for_ (playerToModel gameId Nothing mUserId player) $ \pl ->
          Db.insert_ pl

  pure PNNewResp{..}

active
  :: (Monad m, MonadError AppError m, MonadReader UserInfo m, Db.Read m)
  => m PNAllResp
active = do
  uId <- asks $ view uiUserId
  rs <- Db.select $ from $ \(g `InnerJoin` p) -> do
    where_ $ (g ^. GameFkUser ==. val uId)
         &&. (g ^. GameState ==. val Active)
         &&. (p ^. PlayerFkGame ==. g ^. GameId)
    pure (g, p)
  _arespGame <- for (singleCollectSnd rs) $ \(Entity _ g, pls) -> do
    players <- for pls $ \(Entity _ pl) ->
      case playerFromModel (Model.gameKey g) pl of
        Just p  -> pure p
        Nothing -> throwError $ ErrBug "playerFromModel failed"
    pure $ gameFromModel players g
  pure PNAllResp{..}

del
  :: (Db.Update m, MonadReader UserInfo m, MonadError AppError m)
  => PNDeleteRq
  -> m ()
del PNDeleteRq{..} = do
  uId <- asks $ view uiUserId
  n <- Db.updateCount $ \g -> do
    set g [ GameState =. val (Aborted "Aborted by player") ]
    where_ $ (g ^. GameFkUser ==. val uId)
         &&. (g ^. GameKey ==. val _drqKey)
  when (n == 0) $ throwError $ ErrDatabase "Game not found"
