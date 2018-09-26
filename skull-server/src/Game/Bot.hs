{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Game.Bot where

import           Prelude              (Bool (..), Double, Int, Maybe (..), any,
                                       filter, fromIntegral, length, max, not,
                                       otherwise, pure, ($), (+), (-), (.), (/),
                                       (<), (<$>), (==), (>), (||))

import           Control.Lens         (use, (&), (+=), (-~), (.=), (.~), (^.))
import           Control.Monad        (when)
import           Control.Monad.Except (Except, throwError)
import           Control.Monad.Random (MonadRandom, getRandom)
import           Data.List            (partition)
import           Data.Monoid          ((<>))
import           Safe                 (lastMay)

import           Control.Monad.State  (get)
import           Data.Foldable        (all, for_)

import           Game.Agent           (agentDo, agentDoBot)
import           Game.Api.Types       (GameError (..))
import           Game.Moves           (placeBet, placeBetFold, playPlain,
                                       playSkull)
import           Game.Play            (getMaxBetValue, withBot, withPlayer,
                                       withPlayer')
import           Game.Play.Types      (WithGame)
import           Game.Types           (Agent, Bid, Card (..), CardFace (..),
                                       CardKind (..), Phase (..), Player, aHand,
                                       aHandLimit, aStack, gPhase, gPlayers,
                                       gRound, hHasSkull, hNumPlains, plAgent,
                                       plAlive, stCards)

botMoves
  :: [Player]
  -> WithGame ()
botMoves players = do
  phase <- use gPhase
  for_ players $ case phase of
    FirstCard ->
      withBot $ agentDoBot playCard
    CardOrBet ->
      moveCardOrBet
    Bet highest -> moveBet highest
    Reveal bid -> moveReveal bid
    RoundFinished -> pure $ pure ()

numCardsHand
  :: Player
  -> Int
numCardsHand player =
  let s = if player ^. plAgent . aHand . hHasSkull
          then 1
          else 0
  in  s + player ^. plAgent . aHand . hNumPlains

numStack
  :: Player
  -> Int
numStack player = length $ player ^. plAgent . aStack . stCards

stackHasSkull
  :: Player
  -> Bool
stackHasSkull player =
  any isSkull $ player ^. plAgent . aStack . stCards

isSkull
  :: Card
  -> Bool
isSkull card =
  _cardKind card == Skull

moveReveal
  :: Bid
  -> Player
  -> WithGame ()
moveReveal bid player = do
  result <- withPlayer' (reveal bid) player
  case result of
    RevealSkull -> do -- TODO: round lost
      withPlayer reduceHandLimit player
      gPhase .= RoundFinished
      -- TODO: test if someone won (only one player remaining)
      gRound += 1
      -- TODO: reset all players
  let nRemaining = bid - numStack player
  when (nRemaining > 0) $ do
    players <- use gPlayers
    -- TODO: modify
    gPlayers .= players

reduceHandLimit
  :: Player
  -> Except GameError Player
reduceHandLimit player = pure $ player &
  case player ^. plAgent . aHandLimit of
    1 -> plAlive .~ False
    _ -> do
      plAgent . aHandLimit -~ 1

data RevealResult
  = RevealSkull
  | RevealNoSkullYet Int -- remaining cards to be flipped
  | RevealSuccess

reveal
  :: Int
  -> Player
  -> Except GameError (Player, RevealResult)
reveal _ player | getLatestRevealedCard player == Just Skull =
  pure (player, RevealSkull)
reveal 0 player =
  pure (player, RevealSuccess)
reveal n player | isAllRevealed player =
  pure (player, RevealNoSkullYet n)
reveal n player = do
  newPlayer <- agentDo revealOwnCard player
  reveal (n - 1) newPlayer

getLatestRevealedCard
  :: Player
  -> Maybe CardKind
getLatestRevealedCard player =
    lastMay $ _cardKind <$> filter isRevealed (player ^. plAgent . aStack . stCards)
  where
    isRevealed card = _cardFace card == FaceUp

isAllRevealed
  :: Player
  -> Bool
isAllRevealed player =
  all (== FaceUp) $ _cardFace <$> player ^. plAgent . aStack . stCards

revealOwnCard
  :: Agent
  -> Agent
revealOwnCard agent =
  let (ups, downs) = partition (\c -> _cardFace c == FaceUp) $ agent ^. aStack . stCards
      (card:rem) = downs
      faceUp = card { _cardFace = FaceUp }
      newCards = ups <> (faceUp : rem)
  in  agent & aStack . stCards .~ newCards

moveCardOrBet
  :: Player
  -> WithGame ()
moveCardOrBet player = do
  playCardProb <- case numCardsHand player of
        0 -> pure (0 :: Double)
        1 -> pure 0.1
        2 -> pure 0.3
        3 -> pure 0.6
        _ -> throwError $ GameError "more than 4 cards in hand"
  dye <- getRandom -- TODO: depend on player position
  if dye < playCardProb
    then withBot (agentDoBot playCard) player
    else moveBet 0 player

moveBet
  :: Int
  -> Player
  -> WithGame ()
moveBet highestBid player = do
  -- place bet
  let bluffProb = 0.2 :: Double
  dye <- getRandom
  -- either no skull in stack, or bluffing anyway:
  -- play as if no skull in stack
  if dye < bluffProb || not (stackHasSkull player)
    then do
      dye <- getRandom :: WithGame Double
      -- optimal bet is: my (safe) card + 1 or + 2
      -- for less predictability, randomly modify optimal bet
      let modifier | dye < 0.1 = 3
                   | dye < 0.2 = 0
                   | dye < 0.6 = 1 -- 40% chance for +1
                   | otherwise = 2 -- 40% chance for +2
      game <- get
      -- never bet more than allowed
      -- TODO: whitelist players without skull
      --       blacklist players with skull only
      let value = max (numStack player + modifier) (getMaxBetValue game)
      if value > highestBid
          then do
            withPlayer (agentDo $ placeBet value) player
            gPhase .= Bet value
          else withPlayer (agentDo placeBetFold) player
    else
      -- skull in stack and not bluffing: fold
      withPlayer (agentDo placeBetFold) player

playCard
  :: MonadRandom m
  => Agent
  -> m Agent
playCard a =
  if a ^. aHand . hHasSkull
  then playRandom a
  else pure $ playPlain a

playRandom
  :: MonadRandom m
  => Agent
  -> m Agent
playRandom a = do
  let prob :: Double
      prob = 1 / (1 + fromIntegral (a ^. aHand . hNumPlains))
  r <- getRandom
  pure $ if r < prob
    then playSkull a
    else playPlain a
