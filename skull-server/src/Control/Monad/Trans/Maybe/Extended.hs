module Control.Monad.Trans.Maybe.Extended
  ( module Control.Monad.Trans.Maybe
  , maybeT
  ) where

import           Control.Monad.Trans.Maybe

maybeT :: Monad m => Maybe a -> MaybeT m a
maybeT = MaybeT . pure
