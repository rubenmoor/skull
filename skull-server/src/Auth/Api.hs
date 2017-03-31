{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
-- |

module Auth.Api where

import           Servant

import           Auth.Api.Types
import           Auth.Types     (SessionProtect)

type Routes =
       "user" :> "new" :> ReqBody '[JSON] UserNewRequest :> Post '[JSON] UserNewResponse -- todo: login
  :<|> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] LoginResponse -- todo: session key
  :<|> SessionProtect :> "logout" :> Get '[JSON] ()
