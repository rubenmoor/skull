module HttpApp.Handler where

import           Handler.Types           (AppEnv, hoistApp)
import           Servant                 ((:<|>) (..), Server)

import           Auth                    (hoistAuthApp)
import qualified HttpApp.Api             as Api
import qualified HttpApp.BotKey.Handler  as BotKey.Handler
import qualified HttpApp.PlayNow.Handler as PlayNow.Handler
import qualified HttpApp.User.Handler    as User.Handler

handlers :: AppEnv -> Server Api.Routes
handlers env =
       hoistApp env User.Handler.public
  :<|> hoistAuthApp env
       (    User.Handler.protected
       :<|> BotKey.Handler.protected
       :<|> PlayNow.Handler.protected
       )
