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

type Public =
       "new" :> ReqBody '[JSON] UserNewRequest :> Post '[JSON] UserNewResponse
  :<|> "exists" :> ReqBody '[JSON] UserExistsRequest :> Post '[JSON] Bool
  :<|> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] LoginResponse

type Protected =
       "logout" :> Get '[JSON] LogoutResponse
  :<|> "name" :> Get '[JSON] UserNameResponse
