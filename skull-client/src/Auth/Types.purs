module Auth.Types where

import Data.Lens (Prism', prism')
import Data.Maybe (Maybe(..))

import Prelude
import Data.Generic (class Generic)

newtype AuthToken = AuthToken String

derive instance genericAuthToken :: Generic AuthToken

_AuthToken :: Prism' AuthToken String
_AuthToken = prism' AuthToken f
  where
    f (AuthToken a) = Just $ a
