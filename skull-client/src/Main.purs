module Main where

import Prelude
import Root.Types as Root
import Control.Monad.Aff (forkAff)
import Control.Monad.Aff.AVar (makeVar, takeVar)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Rec.Class (forever)
import Data.Argonaut.Generic.Aeson (decodeJson, encodeJson)
import Data.Generic (gShow)
import Halogen (action)
import Halogen.Aff (HalogenEffects, awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX)
import Root (root)
import Servant.PureScript.Settings (SPSettings_(..))
import ServerAPI (SPParams_(..))
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

main :: Eff (HalogenEffects (console :: CONSOLE, ajax :: AJAX)) Unit
main = runHalogenAff do
  body <- awaitBody
  ajaxError <- makeVar
  let env =
        { apiSettings
        , ajaxError
        }
  io <- runUI (root env) unit body
  forkAff $ forever do
    err <- takeVar ajaxError
    io.query $ action $ Root.ShowError err
