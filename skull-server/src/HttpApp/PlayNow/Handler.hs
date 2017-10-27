{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module HttpApp.PlayNow.Handler where

import           Prelude                             hiding (all, round)

import           Control.Lens                        (view)
import           Control.Monad                       (when)
import           Control.Monad.Except                (MonadError, throwError)
import           Control.Monad.IO.Class              (MonadIO, liftIO)
import           Control.Monad.Random                (evalRand, getStdGen)
import           Control.Monad.Reader                (MonadReader, asks)
import           Data.Foldable                       (traverse_)
import           Data.Function                       (on)
import           Data.List                           (sortBy)
import           Data.Traversable                    (for)
import           Database.Esqueleto                  (Value (..), from, val,
                                                      where_, (&&.), (==.),
                                                      (^.))
import           Servant                             ((:<|>) (..), ServerT)
import           System.Entropy                      (getEntropy)
import           TextShow                            (showt)

import           Auth.Types                          (UserInfo (..), uiUserId)
import qualified Data.ByteString.Base64.URL.Extended as Base64
import           Data.Functor.Extended               (forEach, (<$>))
import qualified Database.Class                      as Db
import           Game
import           Game.Agent                          (agentDo)
import           Game.Bot                            (playCard)
import           Handler                             (HandlerProtectedT)
import           HttpApp.Model                       (EntityField (..))
import qualified HttpApp.Model                       as Model
import qualified HttpApp.PlayNow.Api                 as Api
import           HttpApp.PlayNow.Api.Types
import           Types                               (AppError (..))

protected :: ServerT Api.Protected (HandlerProtectedT IO)
protected =
       new
  :<|> all
  :<|> del

new
  :: (Monad m, MonadIO m, MonadError AppError m, MonadReader UserInfo m, Db.Insert m)
  => PNNewRq
  -> m PNNewResp
new PNNewRq{..} = do
  when (_nrqNumPlayers < 2 || _nrqNumPlayers > 4) $
    throwError $ ErrUser "The allowed number of players is between 2 and 4"
  uId <- asks $ view uiUserId
  _gKey <- Base64.encode <$> liftIO (getEntropy 32)
  let _gState = Round 0
      _gPhase = FirstCard
      _gPlayers = undefined -- TODO
      _nrespGame = Game{..}
  gameId <- Db.insert $ gameToModel uId _nrespGame
  humanKey <- Base64.encode <$> liftIO (getEntropy 32)
  let startHand = Hand
        { _hNumPlains = 3
        , _hHasSkull = True
        }
      humanPlayer = Player
        { _plKey = humanKey
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
  let allPlayers = sortBy (compare `on` _plKey) $ humanPlayer : botPlayers
  -- traverse_ Db.insert_ allPlayers
  pure PNNewResp{..}

all
  :: (Monad m, MonadError AppError m, MonadReader UserInfo m, Db.Read m)
  => m PNAllResp
all = do
  uId <- asks $ view uiUserId
  rs <- Db.select $ from $ \g -> do
    where_ $ (g ^. GameFkUser ==. val uId)
         &&. (p ^. PlayerFkGame ==. g ^. GameId)
    pure (g, p)
  _arespGame <- case rs of
    [] -> pure Nothing
    ls -> pure $ Just $ gameFromModel g
  pure PNAllResp{..}

del
  :: (Db.Delete m, MonadReader UserInfo m, MonadError AppError m)
  => PNDeleteRq
  -> m ()
del PNDeleteRq{..} = do
  uId <- asks $ view uiUserId
  n <- Db.deleteCount $ from $ \g ->
    where_ $ (g ^. GameFkUser ==. val uId)
         &&. (g ^. GameKey ==. val _drqKey)
  when (n == 0) $ throwError $ ErrDatabase "Game not found"
