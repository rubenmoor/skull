{-# LANGUAGE Arrows #-}

module Database.Query where

import           Control.Arrow         (returnA)
import           Control.Lens          ((^.))
import           Opaleye

import           Database.Adaptor
import           Database.Schema
import           Database.Schema.Types
import qualified HttpApp.User.Model    as Model
import           HttpApp.User.Types

sessionByKey :: QueryArr SessionKey SessionR
sessionByKey = proc key -> do
  session <- queryTable sessions -< ()
  restrict -< session ^. sessionKey .=== key
  returnA -< session

userAndSessionBySessionKey :: Model.SessionKey -> Query (UserR, SessionR)
userAndSessionBySessionKey key = proc () -> do
  user    <- queryTable users -< ()
  session <- sessionByKey -< pgSessionKey key
  restrict -< user ^. userId .=== session ^. sessionFkUser
  returnA -< (user, session)

userByUserName :: UserName -> Query UserR
userByUserName name = proc () -> do
  user <- queryTable users -< ()
  restrict -< user ^. userName .== pgStrictText name
  returnA -< user

sessionByUserId :: Model.UserId -> Query SessionR
sessionByUserId uId = proc () ->  do
  session <- queryTable sessions -< ()
  restrict -< session ^. sessionFkUser .=== pgUserId uId
  returnA -< session
