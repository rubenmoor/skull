module Main where

import Prelude
import Halogen.Aff as HA
import Network.HTTP.Affjax as AX
import Auth.SignupForm (signupForm)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Halogen.VDom.Driver (runUI)
import Servant.PureScript.Settings (SPSettings_(..))
import ServerAPI (SPParams_(..))
import Data.Argonaut.Generic.Aeson (decodeJson, encodeJson)
import Data.Generic (gShow)
import Types (ApiSettings)

apiSettings :: ApiSettings
apiSettings = SPSettings_
  { encodeJson : encodeJson
  , decodeJson : decodeJson
  , toURLPiece : gShow
  , params : SPParams_
      { baseURL : ""
      }
  }

main :: Eff (HA.HalogenEffects (console :: CONSOLE, ajax :: AX.AJAX)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI (signupForm apiSettings) "" body
