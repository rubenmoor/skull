{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
-- |

module Auth.Api where

import           Servant

import           Auth.Api.Types
import           Auth.Model     (UserName)
import           Auth.Types     (SessionProtect)

type Routes =
       "user" :> "new" :> ReqBody '[JSON] UserNewRequest :> Post '[JSON] UserNewResponse
  :<|> "user" :> "exists" :> ReqBody '[JSON] UserName :> Post '[JSON] Bool
  :<|> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] LoginResponse
  :<|> SessionProtect :> "logout" :> Get '[JSON] ()
