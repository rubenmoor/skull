module BotKey.Handler where

import           Servant          ((:<|>) (..), ServerT)

import           Auth.Types       (UserInfo)
import qualified BotKey.Api       as Api
import           BotKey.Api.Types
import           BotKey.Types
import qualified Database.Class   as Db
import           Handler          (HandlerT)

handlers :: ServerT Api.Routes (HandlerT IO)
handlers =
       botKeyNew
  :<|> botKeyAll

botKeyNew :: Db.Insert m
          => UserInfo
          -> BotKeyNewRequest
          -> m ()
botKeyNew = undefined

botKeyAll :: Db.Read m
          => UserInfo
          -> m [BotKey]
botKeyAll = undefined
