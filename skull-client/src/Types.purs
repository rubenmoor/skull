module Types where

import Basil (STORAGE)
import Control.Monad.Aff.AVar (AVAR, AVar)
import Data.Lens (Lens', lens)
import Network.HTTP.Affjax (AJAX)
import Servant.PureScript.Settings (SPSettings_)
import ServerAPI (SPParams_)

type ApiSettings = SPSettings_ SPParams_

type MkRequestEffects e =
  ( storage :: STORAGE
  , ajax :: AJAX
  , avar :: AVAR
  | e
  )

type UrlRoot = String

newtype Env = Env
  { httpUrlRoot :: UrlRoot
  , ajaxError :: AVar Error
  }

newtype Error = Error
  { title   :: String
  , details :: String
  }

_title :: Lens' Error String
_title =
    lens errorTitle (\(Error r) str -> Error (r { title = str }))
  where
    errorTitle (Error { title }) = title


_details :: Lens' Error String
_details =
    lens errorDetails (\(Error r) str -> Error (r { details = str }))
  where
    errorDetails (Error { details }) = details
