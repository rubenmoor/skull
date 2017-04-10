module Types where

import Control.Monad.Aff.AVar (AVar)
import ErrorMessage.Types (ErrorMessage)
import Servant.PureScript.Settings (SPSettings_(..))
import ServerAPI (SPParams_(..))

type ApiSettings = SPSettings_ SPParams_

type Env =
  { apiSettings :: ApiSettings
  , ajaxError :: AVar ErrorMessage
  }
