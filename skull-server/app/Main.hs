{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Prelude                    hiding (putStrLn)

import           Control.Monad.IO.Class     (liftIO)
import           Data.Default               (def)
import           Data.Monoid                ((<>))
import qualified Data.Text                  as Text
import           Data.Text.IO
import qualified Database.PostgreSQL.Simple as Postgres
import           Diener                     (LogEnv (..), withLogger)
import qualified Network.Wai.Handler.Warp   as Warp
import           Servant
import           TextShow

import           Auth                       (authHandler)
import qualified Auth.Api                   as Auth
import qualified Auth.Handler               as Auth
import           Auth.Types                 (AuthMiddleware)
import qualified BotKey.Api                 as BotKey
import qualified BotKey.Handler             as BotKey
import qualified Game.Api                   as Game
import qualified Game.Handler               as Game
import           Handler                    (transform)
import           Options                    (Options (..), getOptions)
import           Types                      (Env (..))

type Routes = (
              "auth"   :> Auth.Routes
         :<|> "botkey" :> BotKey.Routes
         :<|> "game"   :> Game.Routes
       )
  :<|> Raw

app :: LogEnv Env -> FilePath -> Server Routes
app env path =
       transform env (
              Auth.handlers
         :<|> BotKey.handlers
         :<|> Game.handlers
         )
  :<|> serveDirectory path

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
    putStrLn $ "Listening on port " <> showt optPort <> " ..."
    Warp.run optPort $ serveWithContext (Proxy :: Proxy Routes)
                                        (handlerContext env)
                                        (app env optAssetDir)

runInHandlerEnv :: Postgres.Connection -> (LogEnv Env -> IO a) -> IO a
runInHandlerEnv connection action =
  withLogger def $ \logFn -> liftIO $ do
    let env   = LogEnv logFn (Env connection)
    action env
