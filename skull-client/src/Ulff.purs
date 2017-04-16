module Ulff where

import Basil (STORAGE)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR, AVar)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Except (class MonadError)
import Control.Monad.Free (Free)
import Control.Monad.Reader (class MonadAsk)
import ErrorMessage.Types (ErrorMessage)
import Halogen.Aff (HalogenEffects)
import Network.HTTP.Affjax (AJAX)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad)
import Servant.PureScript.Affjax (AjaxError(..))
import Servant.PureScript.Settings (SPSettings_(..))
import ServerAPI (SPParams_(..))

type UlffEffects = HalogenEffects
  ( ajax :: AJAX
  , console :: CONSOLE
  , storage :: STORAGE
  )

type Settings =
  { httpUrlRoot :: String
  , ajaxErrorAVar :: AVar ErrorMessage
  }

type Request =
     forall eff m result.
       ( MonadAsk (SPSettings_ SPParams_) m
       , MonadError AjaxError m
       , MonadAff ( ajax :: AJAX | eff) m
       )
  => m result

type Result a =
     forall result.
     result
  -> Aff (ajax :: AJAX, avar :: AVAR, storage :: STORAGE) a

data UlffF a
  = MkRequest
      Request
      Result a
  | Log String (Aff (console :: CONSOLE) a)

newtype UlffM a =
  UlffM { unUlffM :: Free UlffF a }

derive newtype instance functorUlffM :: Functor UlffM
derive newtype instance applyUlffM :: Apply UlffM
derive newtype instance applicativeUlffM :: Applicative UlffM
derive newtype instance bindUlffM :: Bind UlffM
derive newtype instance monadUlffM :: Monad UlffM

-- interface

-- mkRequest :: forall m b.
--              MonadTrans m
--           => m UlffM b
