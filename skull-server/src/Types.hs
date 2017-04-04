{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE Rank2Types     #-}
-- |

module Types where

import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple (Connection)
import           Diener                     (LogEnv)

type Env = LogEnv AppEnv

data AppEnv = AppEnv
  { envDbConnection :: Connection
  }

data AppError
  = ErrUser Text
  | ErrBug  Text
  | ErrDatabase Text
  | ErrUnauthorized Text
  | ErrForbidden Text
