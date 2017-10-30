{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Prelude                  hiding (putStrLn)

import           Control.Monad            (when)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Default             (def)
import           Data.Monoid              ((<>))
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Data.Text.IO
import qualified Database.Persist.Sqlite  as Sqlite
import           Diener                   (LogEnv (..), LogLevel, withLogger)
import           Diener.Logger            (Settings (..))
import qualified Network.Wai.Handler.Warp as Warp
import           Servant
import           System.Directory         (doesFileExist)
import           TextShow

import qualified Api
import qualified Game.Handler             as Game
import           Handler                  (transform)
import qualified HttpApp.Handler          as HttpApp
import qualified HttpApp.Model            as Model
import           Options                  (Options (..), getOptions)
import           Types                    (AppEnv (..), Env)

app :: Env -> FilePath -> Server Api.Routes
app env path =
       transform env HttpApp.handlers
  :<|> transform env Game.handlers
  :<|> serveDirectory path

main :: IO ()
main = do
  Options{..} <- getOptions

  putStrLn $ "Using database file " <> showt optDbName
  fileExists <- doesFileExist $ Text.unpack optDbName
  when (not fileExists) $
    Sqlite.runSqlite optDbName $ Sqlite.runMigration Model.migrateAll

  runInHandlerEnv optDbName optLogLevel $ \env -> do
    putStrLn $ "Serving public directory " <> showt optAssetDir
    putStrLn $ "Listening to port " <> showt optPort <> " ..."
    Warp.run optPort $ serve (Proxy :: Proxy Api.Routes)
                             (app env optAssetDir)

runInHandlerEnv :: Text -> LogLevel -> (Env -> IO a) -> IO a
runInHandlerEnv dbName logLevel action =
  withLogger def{ logLevel } $ \logFn ->
    Sqlite.withSqlitePool dbName 1 $ \pool -> do
      liftIO $ action $ LogEnv logFn (AppEnv pool)
