{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module HttpApp.Api
  ( Routes
  , Protected
  ) where

import           Servant             ((:<|>), (:>))

import           Auth.Types          (AuthProtect)
import qualified HttpApp.BotKey.Api  as BotKey.Api
import qualified HttpApp.PlayNow.Api as PlayNow.Api
import qualified HttpApp.User.Api    as User.Api

type Routes =
       "user"   :> User.Api.Public
  :<|> AuthProtect :> Protected

type Protected =
       "user" :> User.Api.Protected
  :<|> "botKey" :> BotKey.Api.Protected
  :<|> "playNow" :> PlayNow.Api.Protected
