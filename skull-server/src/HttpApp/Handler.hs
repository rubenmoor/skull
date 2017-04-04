module HttpApp.Handler where

import           Handler                (HandlerT)
import           Servant                ((:<|>) (..), ServerT)

import           Auth                   (authHandler)
import qualified HttpApp.Api            as Api
import qualified HttpApp.BotKey.Handler as BotKey.Handler
import qualified HttpApp.User.Handler   as User.Handler

handlers :: ServerT Api.Routes (HandlerT IO)
handlers =
       User.Handler.public
  :<|> authHandler
       (    User.Handler.protected
       :<|> BotKey.Handler.protected
       )
