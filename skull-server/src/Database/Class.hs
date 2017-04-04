{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Database.Class where

import           Data.Profunctor.Product.Default (Default)
import           Data.Text                       (Text)
import qualified Opaleye

type Write m = (Delete m, Insert m, Update m)
type ReadWrite m = (Database.Class.Read m, Write m)

class Functor m => Read m where
  getByQuery :: Default Opaleye.QueryRunner columns x
             => Opaleye.Query columns -> m [x]

  getOneByQuery :: Default Opaleye.QueryRunner columns x
                => Opaleye.Query columns -> m (Either Text x)
  getOneByQuery query =
    getByQuery query <&> \case
      []    -> Left "getOneByQuery: empty list"
      _:_:_ -> Left "getOneByQuery: not singleton"
      x:[]  -> Right x

class Delete m where
  deleteWhere :: Opaleye.Table columnsW columnsR
                -> (columnsR -> Opaleye.Column Opaleye.PGBool)
                -> m ()

class Functor m => Insert m where
  insertMany_ :: Opaleye.Table columnsW columnsR -> [columnsW] -> m ()

  insert_ :: Opaleye.Table columnsW columnsR -> columnsW -> m ()
  insert_ table columns = insertMany_ table [columns]

  insertMany :: Default Opaleye.QueryRunner columnsReturned xs
             => Opaleye.Table columnsW columnsR
             -> [columnsW]
             -> (columnsR -> columnsReturned)
             -> m [xs]

  insert :: Default Opaleye.QueryRunner columnsReturned xs
         => Opaleye.Table columnsW columnsR
         -> columnsW
         -> (columnsR -> columnsReturned)
         -> m xs
  insert table columns f = head <$> insertMany table [columns] f

class Update m where
  updateWhere :: Opaleye.Table columnsW columnsR
              -> (columnsR -> columnsW)
              -> (columnsR -> Opaleye.Column Opaleye.PGBool)
              -> m ()

(<&>) :: Functor m => m a -> (a -> b) -> m b
(<&>) = flip fmap
