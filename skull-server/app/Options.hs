-- |

module Options
  ( Options (..)
  , getOptions
  ) where

import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import qualified Data.Text           as Text

import           Options.Applicative

data Options = Options
  { optPort       :: Int
  , optDbHost     :: Text
  , optDbName     :: Text
  , optDbUser     :: Text
  , optDbPassword :: Text
  , optAssetDir   :: FilePath
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
  <$> option auto (  long "port"
                  <> short 'p'
                  <> metavar "PORT"
                  <> value 3000
                  <> showDefault
                  <> help "Server port"
                  )
  <*> txtOption   (  long "postgres-host"
                  <> short 'm'
                  <> metavar "HOSTNAME"
                  <> value "127.0.0.1"
                  <> showDefault
                  <> help "Hostname of Postgres database"
                  )
  <*> txtOption   (  long "database-name"
                  <> short 'd'
                  <> metavar "DBNAME"
                  <> value "test"
                  <> showDefault
                  <> help "Database name for Postgres database"
                  )
  <*> txtOption (  long "database-user"
                <> short 'u'
                <> metavar "DBUSER"
                <> value "skull"
                <> showDefault
                <> help "Username of Postgres database"
                )
  <*> txtOption (  long "database-password"
                <> short 'a'
                <> metavar "PASSWORD"
                <> value "skull"
                <> showDefault
                <> help "Password for user skull of Postgres database")
  <*> strOption (  long "asset-directory"
                <> short 'd'
                <> metavar "ASSETDIR"
                <> value "assets"
                <> showDefault
                <> help "Directory of static assets")
