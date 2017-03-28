{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Database.Postgres.Class where

import           Data.Profunctor.Product.Default (Default)
import qualified Opaleye

class Read m where
  getAll :: Default Opaleye.QueryRunner columns x
         => Opaleye.Query columns -> m [x]

class Insert m where
  insert_ :: Opaleye.Table columns columns' -> [columns] -> m ()
  insert :: Default Opaleye.QueryRunner columnsReturned xs
         => Opaleye.Table columnsW columnsR
         -> [columnsW]
         -> (columnsR -> columnsReturned)
         -> m [xs]
