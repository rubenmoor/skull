{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE Rank2Types     #-}
-- |

module Types where

import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple (Connection)

-- technical types

data Env = Env
  { envDbConnection :: Connection
  }

data AppError
  = ErrUser Text
  | ErrBug  Text
  | ErrDatabase Text
  | ErrUnauthorized Text
  | ErrForbidden Text
