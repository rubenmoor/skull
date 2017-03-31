{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Handler
  ( handlers
  ) where

import           Prelude                hiding (round)

import           Control.Lens
import           Control.Monad.Except   (ExceptT (ExceptT), MonadError,
                                         runExceptT, throwError)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Foldable          (for_)
import           Data.Text              (Text)
import           Servant                ((:<|>) (..), ServerT)

import qualified Api
import           Auth                   (mkPwHash, verifyPassword)
import           Auth.Api.Types
import           Auth.Model             (Session, User)
import           Auth.Types             (UserInfo, uiUserId)
import           BotKey.Api.Types       (BotKeyNewRequest)
import           BotKey.Types           (BotKey)
import           Database.Adaptor       (mkUser)
import qualified Database.Class         as Db
import           Database.Common        (createSession, deleteSession)
import qualified Database.Query         as Query
import           Database.Schema        (users)
import           Database.Schema.Types
import           Game.Api.Types         (AuthInfo (..), ErrorOr,
                                         GameJoinRequest (..), GameState (..),
                                         PlayFirstCard (..))
import           Game.Types             (Card (..), Hand (..))
import           Handler.Types          (HandlerT)
import           Types                  (AppError)

handlers :: ServerT Api.Routes (HandlerT IO)
handlers =
       userNew
  :<|> authLogin
  :<|> authLogout
  :<|> botKeyNew
  :<|> botKeyAll
  :<|> gameJoin
  :<|> playFirstCard

userNew :: (MonadIO m, Db.Read m, Db.Insert m)
        => UserNewRequest
        -> m UserNewResponse
userNew UserNewRequest { unrUserName = name
                       , unrPassword = password
                       } =
  Db.getOneByQuery (Query.userByUserName name) >>= \case
    Right (_ :: User) -> pure $ UserNewFailed "username already exists"
    Left  _           -> do
      pwHash <- mkPwHash password
      uId <- Db.insert users (mkUser name pwHash "") (view userId)
      key <- createSession uId
      pure $ UserNewSuccess name key

authLogin :: (Db.Read m, Db.Delete m, Db.Insert m, MonadIO m)
          => LoginRequest
          -> m LoginResponse
authLogin LoginRequest { lrUserName = name
                       , lrPassword = password
                       } =
    checkLogin >>= \case
      Left  err    -> pure $ LoginFailed err
      Right user -> do
        key <- getSession $ user ^. userId
        pure $ LoginSuccess (user ^. userName) key
  where
    checkLogin = runExceptT $ do
      user <- getUser
      checkPassword_ (user :: User)
      pure user
    getUser = ExceptT $ over _Left (\_ -> "user name unknown") <$>
      Db.getOneByQuery (Query.userByUserName name)
    checkPassword_ user =
      if verifyPassword password (user ^. userPwHash)
        then pure ()
        else throwError "wrong password"

    getSession uId = do
      -- when logged in: log out first
      eSession <- Db.getOneByQuery (Query.sessionByUserId uId)
      for_ (eSession :: Either Text Session) $ \session ->
        deleteSession $ session ^. sessionId
      -- and create brand-new session
      createSession uId

authLogout :: (MonadError AppError m, Db.Read m, Db.Delete m)
           => UserInfo
           -> m ()
authLogout userInfo =
  Db.getOneByQuery (Query.sessionByUserId $ userInfo ^. uiUserId) >>= \case
    Left msg -> throwError $ ErrBug msg
    Right session -> deleteSession $ (session :: Session) ^. sessionId

botKeyNew :: Db.Insert m
              => UserInfo
              -> BotKeyNewRequest
              -> m ()
botKeyNew = undefined

botKeyAll :: Db.Read m
          => UserInfo
          -> m [BotKey]
botKeyAll = undefined

gameJoin :: (Db.Insert m, Monad m)
         => GameJoinRequest
         -> m (ErrorOr GameState)
gameJoin GameJoinRequest { gjrGameId = gameId, gjrBotId = botId } = do
  -- find game by id or throw error
  let numPlayers = undefined
  -- find all associated players
  -- check if there is an empty spot or throw error
  -- create player id
  let playerId = undefined
  -- save botId and playerId in game
  -- poll the database every second until timeout
  -- once the game is full, reply with game state
  pure $ Result $ GameState
    { gsPlayerId = playerId
    , gsRound = 1
    , gsPhase = FirstCard
    , gsMyStack = MyStack []
    , gsHand = Hand { handNumPlains = 3, handHasSkull = True }
    , gsStacks = Stacks $ replicate numPlayers 0
    , gsBets = Bets $ replicate numPlayers 0
    }

playFirstCard :: (Db.Insert m, Monad m)
              => PlayFirstCard
              -> m (ErrorOr GameState)
playFirstCard PlayFirstCard { pfcCard = card, pfcAuth = auth } =
    withError $ do
      let AuthInfo { aiGameId = gameId, aiBotId = botId, aiPlayerId = playerId } = auth
      -- lookup game by id
      let numPlayers = undefined
      -- check if bot and player id are in the game
      let round = undefined
      let hand = undefined
      newHand <- case card of
        Skull -> playSkullOrError hand
        Plain -> playPlainOrError hand
      -- poll the db every second until timeout
      -- once everyone has played, reply with game state
      pure $ GameState
        { gsPlayerId = playerId
        , gsRound = round
        , gsPhase = CardOrBet
        , gsMyStack = MyStack [card]
        , gsHand = newHand
        , gsStacks = Stacks $ replicate numPlayers 1
        , gsBets = Bets $ replicate numPlayers 0
        }
  where
    withError action = either Error Result <$> runExceptT action
    playSkullOrError h@Hand{ handHasSkull = hasSkull} =
      if hasSkull
      then pure $ h { handHasSkull = False }
      else throwError $ GameError "Illegal play: skull. Your hand doesn't have the skull card."
    playPlainOrError h@Hand{ handNumPlains = numPlains } =
      if numPlains > 0
      then pure $ h { handNumPlains = numPlains - 1}
      else throwError $ GameError "Illegal play: plain. Your hand doesn't have a plain card."
