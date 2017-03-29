{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Main where

import           Control.Monad.Except       (ExceptT)
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.ByteString.Lazy       as ByteString.Lazy
import           Data.Default               (def)
import           Data.Monoid                ((<>))
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Database.PostgreSQL.Simple as Postgres
import           Diener                     (LogEnv (..), runDienerT,
                                             withLogger)
import qualified Network.Wai.Handler.Warp   as Warp
import           Servant

import qualified Api
import           Handler                    (handlers)
import           Handler.Types              (HandlerT (..))
import           Options                    (Options (..), getOptions)

import           Types                      (AppError (..), Env (..))


app :: LogEnv Env -> Server Api.Routes
app env = enter transform handlers
  where
    transform :: HandlerT IO :~> ExceptT ServantErr IO
    transform = Nat $ \action ->
        liftIO (runDienerT env $ unHandlerT action) >>= either
          (throwError . appErrToServantErr)
          pure

    appErrToServantErr :: AppError -> ServantErr
    appErrToServantErr = \case
        ErrUser msg     -> err400 { errBody = toBS msg }
        ErrBug msg      -> err500 { errBody = toBS msg }
        ErrDatabase msg -> err500 { errBody = toBS msg }
      where
        toBS = ByteString.Lazy.fromStrict . Text.encodeUtf8

main :: IO ()
main = do
  Options{..} <- getOptions
  connection <- Postgres.connect $ Postgres.defaultConnectInfo
      { Postgres.connectDatabase = Text.unpack optDbName
      , Postgres.connectHost     = Text.unpack optDbHost
      , Postgres.connectUser     = Text.unpack optDbUser
      , Postgres.connectPassword = Text.unpack optDbPassword
      }
  runInHandlerEnv connection $ \env -> do
    putStrLn $ "Listening on port " <> show optPort <> " ..."
    Warp.run optPort $ serve Api.api $ app env

runInHandlerEnv :: Postgres.Connection -> (LogEnv Env -> IO a) -> IO a
runInHandlerEnv connection action =
  withLogger def $ \logFn -> liftIO $ do
    let env   = LogEnv logFn (Env connection)
    action env
