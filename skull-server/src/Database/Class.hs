{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Database.Class where

import           Control.Monad    (void)
import           Data.Text        (Text)

import           Database.Gerippe (EntEqs, ToBack)
import           Database.Persist (Entity, EntityField, Key, PersistEntity,
                                   PersistField)

type Write m = (Delete m, Insert m, Update m)
type ReadWrite m = (Database.Class.Read m, Write m)

class Functor m => Read m where
  get
    :: ToBack a
    => Key a
    -> m (Maybe a)

  getAll
    :: ToBack a
    => m [Entity a]

  getWhere
    :: (ToBack a, PersistField b)
    => EntityField a b
    -> b
    -> m [Entity a]

  getOneWhere
    :: (ToBack a, PersistField b)
    => EntityField a b
    -> b
    -> m (Either Text (Entity a))
  getOneWhere field value =
    getWhere field value <&> \case
      []    -> Left "getOneByQuery: empty list"
      _:_:_ -> Left "getOneByQuery: not singleton"
      x:[]  -> Right x

  joinMTo1Where'
    :: (EntEqs a b, PersistField c)
    => EntityField a (Key b)
    -> EntityField b (Key b)
    -> EntityField a c
    -> c
    -> m [(Entity a, Entity b)]

class Functor m => Insert m where
  insert
    :: (ToBack a, PersistEntity a)
    => a
    -> m (Key a)

  insert_
    :: (ToBack a, PersistEntity a)
    => a
    -> m ()
  insert_ = void . insert

class Delete m where
  delete
    :: (ToBack a, PersistEntity a)
    => Key a
    -> m ()

class Update m where
  update
    :: (ToBack a, PersistEntity a)
    => Key a
    -> a
    -> m ()

(<&>) :: Functor m => m a -> (a -> b) -> m b
(<&>) = flip fmap
