{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE Rank2Types     #-}
-- |

module Types where

import           Data.Text        (Text)
import           Database.Gerippe (ConnectionPool)
import           Diener           (LogEnv)

type Env = LogEnv AppEnv

data AppEnv = AppEnv
  { envDbConnection :: ConnectionPool
  }

data AppError
  = ErrUser Text
  | ErrBug  Text
  | ErrDatabase Text
  | ErrUnauthorized Text
  | ErrForbidden Text
