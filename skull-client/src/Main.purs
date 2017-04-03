module Main where

import Prelude
import Halogen.Aff as HA
import Network.HTTP.Affjax as AX
import Auth.Signup (signup)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Halogen.VDom.Driver (runUI)

main :: Eff (HA.HalogenEffects (console :: CONSOLE, ajax :: AX.AJAX)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI signup unit body
