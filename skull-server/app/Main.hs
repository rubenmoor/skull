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

import qualified Api
import           Handler                    (transform)
import qualified HttpApp.Handler            as HttpApp
import           Options                    (Options (..), getOptions)
import           Types                      (AppEnv (..), Env)

app :: Env -> FilePath -> Server Api.Routes
app env path =
       transform env HttpApp.handlers
  :<|> serveDirectory path

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
    putStrLn $ "Serving public directory " <> showt optAssetDir
    putStrLn $ "Listening on port " <> showt optPort <> " ..."
    Warp.run optPort $ serve (Proxy :: Proxy Api.Routes)
                             (app env optAssetDir)

runInHandlerEnv :: Postgres.Connection -> (Env -> IO a) -> IO a
runInHandlerEnv connection action =
  withLogger def $ \logFn ->
    liftIO $ action $ LogEnv logFn (AppEnv connection)
