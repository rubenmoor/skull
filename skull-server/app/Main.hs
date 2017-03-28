{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Main where

import           Control.Monad.Except     (ExceptT)
import           Control.Monad.IO.Class   (liftIO)
import qualified Data.ByteString.Lazy     as ByteString.Lazy
import           Data.Default             (def)
import           Data.Monoid              ((<>))
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import           Diener                   (LogEnv (..), runDienerT, withLogger)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Opaleye
import           Servant

import qualified Api
import           Handler                  (handlers)
import           Handler.Types            (HandlerT)
import           Options                  (Options (..), getOptions)

import           Types                    (AppError (..), Env (..))


app :: LogEnv Env -> Server Api.Routes
app env = enter transform handlers
  where
    transform :: HandlerT IO :~> ExceptT ServantErr IO
    transform = Nat $ \action ->
      case runExceptT (unHandlerT action) of
        Left msg -> pure $ GameError msg
        Right action' -> liftIO (runDienerT env action') >>= either
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
  connection <- Opaleye.connect $ Opaleye.defaultConnectInfo
      { connectDatabase = Text.unpack optDbName
      , connectHost     = Text.unpack optDbHost
      , connectUser     = Text.unpack optDbUser
      , connectPassword = Text.unpack optDbPassword
      }
  runInHandlerEnv connection $ \env -> do
    putStrLn $ "Listening on port " <> show optPort <> " ..."
    Warp.run optPort $ serve Api.api $ app env

runInHandlerEnv :: Opaleye.Connection -> (LogEnv Env -> IO a) -> IO a
runInHandlerEnv connection action =
  withLogger def $ \logFn -> liftIO $ do
    let env   = LogEnv logFn (Env connection)
    action env
