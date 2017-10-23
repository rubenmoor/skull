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
import Data.Tuple (Tuple(..))
import Halogen (action, hoist, liftEff)
import Halogen.Aff (HalogenEffects, awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX)
import Root (root)
import Router (routing)
import Routing (matchesAff)
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
      io <- runUI (hoist (runUlffT env) $ root httpUrlRoot) unit body
      -- routing
      void $ forkAff do
        Tuple _ new <- matchesAff routing
        io.query $ action $ Root.GotoLocation new
      -- ajax errors
      forkAff $ forever do
        err <- takeVar ajaxError
        io.query $ action $ Root.ShowError err
  where
    getBody = do
      nBody <- liftEff $ window >>= document >>= body
      maybe (throwError $ error "Body not found")
            pure
            nBody
