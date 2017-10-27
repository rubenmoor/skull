{-# LANGUAGE NoImplicitPrelude #-}

module Database.Query
  ( collectSnd
  , singleCollectSnd
  ) where

import           Control.Applicative (pure)
import           Control.Arrow       (second)
import           Control.Monad       (guard)
import           Data.Eq             ((==))
import           Data.Function       (($), (.))
import           Data.List           (map, (++))
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Maybe          (Maybe (..), listToMaybe)
import           Data.Ord            (Ord)
import           Data.Tuple          (snd)

collectSnd
  :: Ord k
  => [(k, v)]
  -> Map k [v]
collectSnd = Map.fromListWith (++) . map (second (:[]))

singleCollectSnd
  :: Ord k
  => [(k,v)]
  -> Maybe (k, [v])
singleCollectSnd ls = do
  let m = collectSnd ls
      vs = map snd ls
  guard $ Map.size m == 1
  (key, _) <- listToMaybe ls
  pure (key, vs)
