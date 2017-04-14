module Main where

import Prelude
import Root.Types as Root
import Control.Monad.Aff (forkAff)
import Control.Monad.Aff.AVar (makeVar, takeVar)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (throwError)
import Control.Monad.Rec.Class (forever)
import DOM.HTML (window)
import DOM.HTML.Document (body)
import DOM.HTML.Window (document)
import Data.Argonaut.Generic.Aeson (decodeJson, encodeJson)
import Data.Generic (gShow)
import Data.Maybe (maybe)
import Data.Nullable (toMaybe)
import Halogen (action, liftEff)
import Halogen.Aff (HalogenEffects, awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX)
import Root (root)
import Servant.PureScript.Settings (SPSettings_(..))
import ServerAPI (SPParams_(..))
import Types (ApiSettings)

apiSettings :: String -> ApiSettings
apiSettings httpUrlRoot = SPSettings_
  { encodeJson : encodeJson
  , decodeJson : decodeJson
  , toURLPiece : gShow
  , params : SPParams_
      { baseURL : httpUrlRoot
      }
  }

main :: String -> Boolean -> Eff (HalogenEffects (console :: CONSOLE, ajax :: AJAX)) Unit
main httpUrlRoot hotReload =
    runHalogenAff do
      body <- if hotReload
         then getBody
         else awaitBody
      ajaxError <- makeVar
      let env =
            { apiSettings: apiSettings httpUrlRoot
            , ajaxError
            }
      io <- runUI (root env) unit body
      forkAff $ forever do
        err <- takeVar ajaxError
        io.query $ action $ Root.ShowError err
  where
    getBody = do
      nBody <- liftEff $ window >>= document >>= body
      maybe (throwError $ error "Body not found")
            pure
            (toMaybe nBody)
