-- |

module Options
  ( Options (..)
  , getOptions
  ) where

import           Control.Monad.Logger     (LogLevel (..))
import           Data.Monoid              ((<>))
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Network.Wai.Handler.Warp (Port)

import           Options.Applicative

data Options = Options
  { optDbName   :: Text
  , optAssetDir :: FilePath -- String
  , optPort     :: Port
  , optLogLevel :: LogLevel
  }

txtOption :: Mod OptionFields String -> Parser Text
txtOption = fmap Text.pack . strOption

getOptions :: IO Options
getOptions = execParser $ info (helper <*> options)
  (  fullDesc
  <> progDesc "Serves the app"
  <> header   "Server"
  )

options :: Parser Options
options = Options
  <$> txtOption   (  long "database-name"
                  <> short 'd'
                  <> metavar "DBNAME"
                  <> value "test.db3"
                  <> showDefault
                  <> help "Database name for Postgres database"
                  )
  <*> strOption (  long "asset-directory"
                <> short 'd'
                <> metavar "ASSETDIR"
                <> value "assets"
                <> showDefault
                <> help "Directory of static assets")
  <*> option auto ( long "port"
                 <> short 'p'
                 <> metavar "PORT"
                 <> value 3000
                 <> showDefault
                 <> help "Port where the server listens"
                 )
  <*> option auto ( long "log-level"
                 <> short 'l'
                 <> metavar "LOGLEVEL"
                 <> value LevelInfo
                 <> showDefault
                 <> help "LevelDebug, LevelInfo, LevelWarn, LevelError"
                  )
