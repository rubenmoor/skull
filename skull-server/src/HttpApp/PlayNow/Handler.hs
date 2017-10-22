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
import           Control.Monad.Reader                (MonadReader, asks)
import           Data.List                           (sort)
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
import           Game.Types
import           Handler                             (HandlerProtectedT)
import           HttpApp.Model                       (EntityField (..),
                                                      Game (..))
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
  gameKey <- Base64.encode <$> liftIO (getEntropy 32)
  humanKey <- showt . Base64.encode <$> liftIO (getEntropy 32)
  let startHand = Hand
        { _hNumPlains = 3
        , _hHasSkull = True
        }
      humanPlayer = Player
        { _plKey = humanKey
        , _plNature = Human
        , _plVictory = None
        , _plHand = startHand
        , _plAlive = True
        , _plStack = Stack []
        , _plBetState = NothingYet
        }
  plKeys <- map (showt . Base64.encode) <$>
    for [(1 :: Int)..3] (\_ -> liftIO $ getEntropy 32)
  let players = forEach plKeys $ \key -> humanPlayer
        { _plKey = key
        , _plNature = Bot
        }
      _giPlayers = sort $ humanPlayer : players
      _giKey = showt gameKey
      _giState = Round 0
      _giPhase = FirstCard
      _nrespInfo = Info{..}
  Db.insert_ Game
    { gameFkUser = uId
    , gameInfo = _nrespInfo
    , gameKey
    }
  pure PNNewResp{..}

all
  :: (Monad m, MonadError AppError m, MonadReader UserInfo m, Db.Read m)
  => m PNAllResp
all = do
  uId <- asks $ view uiUserId
  rs <- Db.select $ from $ \g -> do
    where_ $ g ^. GameFkUser ==. val uId
    pure $ g ^. GameInfo
  _arespInfo <- case rs of
    []         -> pure Nothing
    [Value gi] -> pure $ Just gi
    _:_:_      -> throwError $ ErrDatabase "found more than one game"
  pure PNAllResp{..}

del
  :: (Db.Delete m, MonadReader UserInfo m, MonadError AppError m)
  => PNDeleteRq
  -> m ()
del PNDeleteRq{..} = do
  uId <- asks $ view uiUserId
  let gameKey = Base64.fromText _drqKey
  n <- Db.deleteCount $ from $ \g ->
    where_ $ (g ^. GameFkUser ==. val uId)
         &&. (g ^. GameKey ==. val gameKey)
  when (n == 0) $ throwError $ ErrDatabase "Game not found"
