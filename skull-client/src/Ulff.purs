module Ulff where

import Auth.Types (AuthToken(..))
import Basil (STORAGE, getSessionKey)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR, putVar)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Either (either)
import Data.Maybe (fromMaybe)
import Network.HTTP.Affjax (AJAX)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, type (~>), Unit, bind, flip, ($), (<<<), (=<<))
import Servant.PureScript.Affjax (AjaxError, errorToString)
import Servant.PureScript.Settings (SPSettings_, defaultSettings)
import ServerAPI (SPParams_(..))
import Types (Env(..), Error(..))

newtype UlffT m a = UlffT (ReaderT Env m a)

type Ulff effects = UlffT (Aff effects)

unUlffT :: forall m. UlffT m ~> ReaderT Env m
unUlffT (UlffT m) = m

derive newtype instance functorUlffT :: Functor m => Functor (UlffT m)
derive newtype instance applyUlffT :: Monad m => Apply (UlffT m)
derive newtype instance applicativeUlffT :: Monad m => Applicative (UlffT m)
derive newtype instance bindUlffT :: Monad m => Bind (UlffT m)
derive newtype instance monadUlffT :: Monad m => Monad (UlffT m)
derive newtype instance monadAskUlffT :: Monad m => MonadAsk Env (UlffT m)

instance monadTransUlffT
      :: MonadTrans UlffT where
  lift = UlffT <<< lift

instance monadEffUlffT
      :: MonadEff eff m
      => MonadEff eff (UlffT m) where
  liftEff = lift <<< liftEff

instance monadAffUlffT
      :: MonadAff eff m
      => MonadAff eff (UlffT m) where
  liftAff = lift <<< liftAff

--

runUlffT :: forall eff.
            Env
         -> UlffT (Aff eff) ~> Aff eff
runUlffT env =
    flip runReaderT env <<< unUlffT

-- interface

mkRequest :: forall eff stack m result.
             ( Monad (stack m)
             , MonadTrans stack
             , MonadAff (ajax :: AJAX, avar :: AVAR, storage :: STORAGE | eff) m
             , MonadAsk Env m
             )
          => (forall api eff'.
                ( MonadAsk (SPSettings_ SPParams_) api
                , MonadError AjaxError api
                , MonadAff ( ajax :: AJAX | eff') api
                )
                => api result
             )
          -> (result -> stack m Unit)
          -> stack m Unit
mkRequest apiCall callback =
    either showError callback =<< lift do
      mSessionKey <- getSessionKey
      Env { httpUrlRoot } <- ask
      let apiSettings = defaultSettings $ SPParams_
            { baseURL : httpUrlRoot
            -- in case of a auth-protected request by a public component,
            -- let the ajax request fail and report the error
            , authToken: AuthToken $ fromMaybe "" mSessionKey
            }
      runExceptT (flip runReaderT apiSettings apiCall)
  where
    showError err = lift do
      Env { ajaxError } <- ask
      liftAff $ putVar ajaxError $ Error
        { title: "Ajax Error"
        , details: errorToString err
        }
