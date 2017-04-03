{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Main where

import           Control.Monad.IO.Class     (liftIO)
import           Data.Default               (def)
import           Data.Monoid                ((<>))
import qualified Data.Text                  as Text
import qualified Database.PostgreSQL.Simple as Postgres
import           Diener                     (LogEnv (..), withLogger)
import qualified Network.Wai.Handler.Warp   as Warp
import           Servant

import qualified Api
import           Auth                       (authHandler)
import           Auth.Types                 (AuthMiddleware)
import           Handler                    (handlers)
import           Handler.Types              (transform)
import           Options                    (Options (..), getOptions)
import           Types                      (Env (..))


app :: LogEnv Env -> Server Api.Routes
app env = (transform env) handlers

handlerContext :: LogEnv Env -> Context (AuthMiddleware ': '[])
handlerContext env = authHandler env :. EmptyContext

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
    Warp.run optPort $ serveWithContext Api.api (handlerContext env) $ app env

runInHandlerEnv :: Postgres.Connection -> (LogEnv Env -> IO a) -> IO a
runInHandlerEnv connection action =
  withLogger def $ \logFn -> liftIO $ do
    let env   = LogEnv logFn (Env connection)
    action env
