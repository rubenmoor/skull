module Basil
  ( getSessionKey
  , setSessionKey
  , STORAGE
  ) where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Prelude (Unit, ($), (<$>), (<<<))

sessionKeyStr :: String
sessionKeyStr = "sessionKey"

getSessionKey :: forall eff m.
                 MonadEff (storage :: STORAGE | eff) m
              => m (Maybe String)
getSessionKey = toMaybe <$> liftEff (basilGet sessionKeyStr)

setSessionKey :: forall eff m.
                 MonadEff (storage :: STORAGE | eff) m
              => String
              -> m Unit
setSessionKey = liftEff <<< basilSet sessionKeyStr

foreign import data STORAGE :: !

foreign import basilSet
  :: forall eff.
     String
  -> String
  -> Eff (storage :: STORAGE | eff) Unit

foreign import basilGet
  :: forall eff.
     String
  -> Eff (storage :: STORAGE | eff) (Nullable String)
