-- |

module Database.Common where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Time.Clock        as Clock
import           Opaleye                ((.===))
import           System.Entropy         (getEntropy)

import           Auth.Types             (sessionLength)
import           Database.Adaptor       (mkSession, pgSessionId, sessionRW)
import qualified Database.Class         as Db
import           Database.Schema        (SessionW, sessions)
import           Database.Schema.Types  (SessionKey' (SessionKey), _sessionId)
import           HttpApp.User.Model
import qualified Util.Base64            as Base64

deleteSession :: Db.Delete m
              => SessionId -> m ()
deleteSession sId =
  Db.deleteWhere sessions ((.=== pgSessionId sId) . _sessionId)

updateSession :: Db.Update m
              => SessionId
              -> (SessionW -> SessionW)
              -> m ()
updateSession sId f =
  Db.updateWhere sessions (f . sessionRW)
                          ((.=== pgSessionId sId) . _sessionId)

createSession :: (MonadIO m, Db.Insert m)
              => UserId
              -> m SessionKey
createSession uId = do
  key <- SessionKey . Base64.encode <$> liftIO (getEntropy 32)
  expiry <- liftIO $ Clock.addUTCTime sessionLength <$> Clock.getCurrentTime
  Db.insert_ sessions $ mkSession uId key expiry
  pure key
