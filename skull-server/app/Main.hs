{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where


import           Prelude                  hiding (putStrLn)

import           Control.Monad            (unless)

import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Logger     (LogLevel)
import           Data.Default             (def)
import           Data.Monoid              ((<>))
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Data.Text.IO
import qualified Database.Persist.Sqlite  as Sqlite
import qualified Network.Wai.Handler.Warp as Warp
import           Servant
import           System.Directory         (doesFileExist)
import           TextShow

import qualified Api
import           Auth                     (hoistAuthAppBotKey)
import qualified Game.Handler             as Game
import           Handler.Types            (AppEnv (..))
import qualified HttpApp.Handler          as HttpApp
import qualified HttpApp.Model            as Model
import           Logger                   (Settings (..), withLogger)
import           Options                  (Options (..), getOptions)

app :: AppEnv -> FilePath -> Server Api.Routes
app env path =
       HttpApp.handlers env
  :<|> hoistAuthAppBotKey env Game.handlers
  :<|> serveDirectoryFileServer path

main :: IO ()
main = do
  Options{..} <- getOptions

  putStrLn $ "Database filename " <> showt optDbName
  fileExists <- doesFileExist $ Text.unpack optDbName
  unless fileExists $ do
    putStrLn "Creating new database"
    Sqlite.runSqlite optDbName $ Sqlite.runMigration Model.migrateAll

  runInHandlerEnv optDbName optLogLevel $ \env -> do
    putStrLn $ "Serving public directory " <> showt optAssetDir
    putStrLn $ "Listening to port " <> showt optPort <> " ..."
    Warp.run optPort $ serve (Proxy :: Proxy Api.Routes)
                             (app env optAssetDir)

runInHandlerEnv :: Text -> LogLevel -> (AppEnv -> IO a) -> IO a
runInHandlerEnv dbName logLevel action =
  withLogger def{ logLevel } $ \envLogFn ->
    Sqlite.withSqlitePool dbName 1 $ \envDbConnection ->
      liftIO $ action AppEnv {..}
