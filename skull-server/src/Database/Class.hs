{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Database.Class where

import           Control.Monad                        (void)
import           Data.Int                             (Int64)

import           Database.Esqueleto                   (Entity, Key,
                                                       PersistEntity,
                                                       SqlBackend, SqlEntity,
                                                       SqlExpr, SqlQuery,
                                                       ToBackendKey)
import           Database.Esqueleto.Internal.Language (Insertion)
import           Database.Esqueleto.Internal.Sql      (SqlSelect)

type Write m = (Delete m, Insert m, Update m)
type ReadWrite m = (Database.Class.Read m, Write m)

class Functor m => Read m where
  get
    :: ToBackendKey SqlBackend a
    => Key a
    -> m (Maybe a)

  select
    :: SqlSelect a r
    => SqlQuery a
    -> m [r]

class Functor m => Insert m where
  insert
    :: (ToBackendKey SqlBackend a, PersistEntity a)
    => a
    -> m (Key a)

  insert_
    :: (ToBackendKey SqlBackend a, PersistEntity a)
    => a
    -> m ()
  insert_ = void . insert

  insertSelect
    :: PersistEntity a
    => SqlQuery (SqlExpr (Insertion a))
    -> m ()

class Delete m where
  deleteByKey
    :: (ToBackendKey SqlBackend a, PersistEntity a)
    => Key a
    -> m ()

  delete
    :: SqlQuery ()
    -> m ()

  deleteCount
    :: SqlQuery ()
    -> m Int64

class Update m where
  update
    :: SqlEntity val
    => (SqlExpr (Entity val) -> SqlQuery ())
    -> m ()

  updateCount
    :: SqlEntity val
    => (SqlExpr (Entity val) -> SqlQuery ())
    -> m Int64

(<&>) :: Functor m => m a -> (a -> b) -> m b
(<&>) = flip fmap
