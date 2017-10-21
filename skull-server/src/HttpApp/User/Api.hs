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
       "new" :> ReqBody '[JSON] UserNewRq :> Post '[JSON] UserNewResp
  :<|> "exists" :> ReqBody '[JSON] UserExistsRq :> Post '[JSON] Bool
  :<|> "login" :> ReqBody '[JSON] LoginRq :> Post '[JSON] LoginResp

type Protected =
       "logout" :> Get '[JSON] LogoutResp
  :<|> "name" :> Get '[JSON] UserNameResp
