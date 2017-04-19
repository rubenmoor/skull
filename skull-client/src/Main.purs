module Main where

import Prelude
import Root.Types as Root
import Basil (STORAGE)
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
import Data.Maybe (maybe)
import Data.Nullable (toMaybe)
import Halogen (action, hoist, liftEff)
import Halogen.Aff (HalogenEffects, awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX)
import Root (root)
import Types (Env(..))
import Ulff (runUlffT)

type Effects = HalogenEffects
  ( ajax :: AJAX
  , console :: CONSOLE
  , storage :: STORAGE
  )

main :: String
     -> Boolean
     -> Eff Effects Unit
main httpUrlRoot hotReload =
    runHalogenAff do
      body <- if hotReload
         then getBody
         else awaitBody
      ajaxError <- makeVar
      let env = Env
            { httpUrlRoot
            , ajaxError
            }
      io <- runUI (hoist (runUlffT env) root) unit body
      forkAff $ forever do
        err <- takeVar ajaxError
        io.query $ action $ Root.ShowError err
  where
    getBody = do
      nBody <- liftEff $ window >>= document >>= body
      maybe (throwError $ error "Body not found")
            pure
            (toMaybe nBody)
