{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
-- |

module HttpApp.User.Api
  ( Public
  , Protected
  , module HttpApp.User.Api.Types
  ) where

import           Servant                ((:<|>), (:>), Get, JSON, Post, ReqBody)

import           HttpApp.User.Api.Types
import           HttpApp.User.Types     (UserName)

type Public =
       "new" :> ReqBody '[JSON] UserNewRequest :> Post '[JSON] UserNewResponse
  :<|> "exists" :> ReqBody '[JSON] UserName :> Post '[JSON] Bool
  :<|> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] LoginResponse

type Protected =
       "logout" :> Get '[JSON] ()
  :<|> "name" :> Get '[JSON] UserName
