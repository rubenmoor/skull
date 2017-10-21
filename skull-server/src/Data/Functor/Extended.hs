module Data.Functor.Extended
  ( module Data.Functor
  , forEach
  ) where

import           Data.Functor

forEach
  :: Functor m
  => m a
  -> (a -> b)
  -> m b
forEach = flip fmap
