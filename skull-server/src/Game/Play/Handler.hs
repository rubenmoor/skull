{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Game.Play.Handler where

import           Prelude              (IO, flip, pure, ($), (+), (<), (<$>),
                                       (>), (>>=))

import           Control.Lens         (use, view, (.=), (^.))
import           Control.Monad        (when)
import           Control.Monad.Except (MonadError, throwError)
import           Control.Monad.RWS    (ask, get)

import           Control.Monad.Random (MonadRandom)

import           Control.Monad.Reader (MonadReader)
import           Data.Monoid          ((<>))
import           TextShow             (showt)

import           Servant              ((:<|>) (..), ServerT)

import           Auth                 (UserInfo)
import qualified Database.Class       as Db
import           Handler.Types        (AppError (..), HandlerAuthT)

import           Game.Agent           (agentDo)
import           Game.Api.Types       (ErrorOr (..), GameError (..))
import           Game.Bot             (botMoves)
import qualified Game.Moves           as Moves
import           Game.Play            (getMaxBetValue, withGame, withPlayer)
import qualified Game.Play.Api        as Api
import           Game.Play.Api.Types
import           Game.Play.Types      (Seating, seatLeft, seatMe, seatRight)
import           Game.Types

handlers :: ServerT Api.Routes (HandlerAuthT IO)
handlers =
         playCard
    :<|> placeBet

placeBet
  :: (Db.Read m, Db.Replace m, MonadError AppError m, MonadReader UserInfo m, MonadRandom m)
  => PlaceBetRq
  -> m (ErrorOr Game)
placeBet req = withGame (req ^. pbrqAuth) $ do
  seating <- ask
  min <- use gPhase >>= \case
    CardOrBet -> pure 1
    Bet n    -> pure $ n + 1
    _ -> throwError $ GameError "placeBet in wrong phase"
  max <- getMaxBetValue <$> get
  when (req ^. pbrqValue < min) $
    throwError $ GameError $ "Bet value must be bigger or equal to " <> showt min
  when (req ^. pbrqValue > max) $
    throwError $ GameError $ "Bet value must be smaller or equal to " <> showt max
  flip withPlayer (seating ^. seatMe) $ agentDo $ Moves.placeBet $ req ^. pbrqValue
  botMoves $ nextInLine seating

playCard
  :: (Db.Read m, Db.Replace m, MonadError AppError m, MonadReader UserInfo m, MonadRandom m)
  => PlayCardRq
  -> m (ErrorOr Game)
playCard req = withGame (req ^. pcrqAuth) $ do
  seating <- ask
  next <- use gPhase >>= \case
    FirstCard -> do gPhase .= CardOrBet
                    view seatLeft
    CardOrBet -> pure $ nextInLine seating
    _         -> throwError $ GameError "playCard in wrong phase"
  flip withPlayer (seating ^. seatMe) $
    agentDo $ case req ^. pcrqCardKind of
      Plain -> Moves.playPlain
      Skull -> Moves.playSkull
  botMoves next

nextInLine
  :: Seating
  -> [Player]
nextInLine seating =
  seating ^. seatRight <> seating ^. seatLeft
