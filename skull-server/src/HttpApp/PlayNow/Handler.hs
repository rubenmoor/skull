{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module HttpApp.PlayNow.Handler where

import           Control.Lens                        (view)
import           Control.Monad                       (when)
import           Control.Monad.Except                (MonadError, runExceptT,
                                                      throwError)
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Control.Monad.Random                (MonadRandom)

import           Control.Monad.Reader                (MonadReader, asks)
import           Control.Monad.Trans.Maybe           (MaybeT (..), runMaybeT)
import           Data.Foldable                       (for_)
import           Data.List                           (sort)
import           Data.Monoid                         ((<>))
import           Data.Traversable                    (for)
import           Database.Esqueleto                  (Entity (..),
                                                      InnerJoin (..), from, set,
                                                      val, where_, (&&.), (=.),
                                                      (==.), (^.))
import           Servant                             ((:<|>) (..), ServerT)
import           System.Entropy                      (getEntropy)
import           TextShow                            (showt)

import           Auth.Types                          (UserInfo (..), uiUserId)
import qualified Data.ByteString.Base64.URL.Extended as Base64
import           Data.Functor.Extended               ((<$>))
import qualified Database.Class                      as Db
import           Database.Query                      (singleCollectSnd)
import           Game
import           Game.Agent                          (agentDo)
import qualified Game.Bot                            as Bot
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
  :: (Monad m, MonadIO m, MonadError AppError m, MonadReader UserInfo m, Db.Insert m, MonadRandom m)
  => PNNewRq
  -> m PNNewResp
new PNNewRq{..} = do
  when (_nrqNumPlayers < 2 || _nrqNumPlayers > 4) $
    throwError $ ErrUser "The allowed number of players is between 2 and 4"

  -- game key

  _gKey <- Base64.encode <$> liftIO (getEntropy 32)

  -- players

  humanKey <- Base64.encode <$> liftIO (getEntropy 32)
  let humanAgent = Agent
        { _aHand = startHand
        , _aStack = Stack []
        , _aBetState = NothingYet
        }
      humanPlayer = Player
        { _plKey = humanKey
        , _plGameKey = _gKey
        , _plKind = HumanPlayNow
        , _plVictory = None
        , _plAlive = True
        , _plAgent = humanAgent
        }
  botPlayers <- for [(1 :: Int)..(_nrqNumPlayers - 1)] $ \_ -> do
    key <- Base64.encode <$> liftIO (getEntropy 32)
    let p = humanPlayer -- make bots by copying the human player
          { _plKey = key
          , _plKind = BotLaplace
          }
    ePlayer <- runExceptT $ agentDo p Bot.playCard
    case ePlayer of
      Left  gameError -> throwError $ ErrBug $ "illegal move on set-up: " <> showt gameError
      Right player -> pure player
  let allPlayers = sort $ humanPlayer : botPlayers

  -- game

  uId <- asks $ view uiUserId
  let _gState = Active
      _gPhase = FirstCard
      _gRound = 0
      _gPlayers = allPlayers
      game = Game{..}

  -- persist

  gameId <- Db.insert $ gameToModel uId game
  for_ allPlayers $ \player ->
    let mUserId = case view plKind player  of
                    HumanPlayNow -> Just uId
                    _            -> Nothing
    in  for_ (playerToModel gameId Nothing mUserId player) $ \pl ->
          Db.insert_ pl

  pure PNNewResp
    { _nrespGame = game
    , _nrespPlayerKey = humanKey
    }

active
  :: (Monad m, MonadError AppError m, MonadReader UserInfo m, Db.Read m)
  => m PNActiveResp
active = do
  uId <- asks $ view uiUserId
  rs <- Db.select $ from $ \(g `InnerJoin` p) -> do
    where_ $ (g ^. GameFkUser ==. val uId)
         &&. (g ^. GameState ==. val Active)
         &&. (p ^. PlayerFkGame ==. g ^. GameId)
    pure (g, p)
  mPNActive <- runMaybeT $ do
    (Entity _ g, pls) <- MaybeT . pure $ singleCollectSnd rs
    players <- for pls $ \(Entity _ pl) ->
      case playerFromModel (Model.gameKey g) pl of
        Just p  -> pure p
        Nothing -> throwError $ ErrBug "playerFromModel failed"
    human <- MaybeT . pure $ findHumanPlayer players
    let game = gameFromModel players g
    pure PNActive
      { _activeGame = game
      , _activePlayerKey = view plKey human
      }
  pure $ PNActiveResp mPNActive

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
