module Handler
  ( handlers
  ) where

import           Servant        ((:<|>) (..), ServerT)

import qualified Api
import qualified Auth.Handler   as Auth
import qualified BotKey.Handler as BotKey
import qualified Game.Handler   as Game
import           Handler.Types  (HandlerT)

handlers :: ServerT Api.Routes (HandlerT IO)
handlers =
       Auth.handlers
  :<|> BotKey.handlers
  :<|> Game.handlers
