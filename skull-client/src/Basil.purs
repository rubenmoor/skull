module Basil
  ( getSessionKey
  , setSessionKey
  , clearSessionKey
  , STORAGE
  ) where

import Control.Monad.Eff (Eff, kind Effect)
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

clearSessionKey :: forall eff m.
                   MonadEff (storage :: STORAGE | eff) m
                => m Unit
clearSessionKey = liftEff $ basilRemove sessionKeyStr

foreign import data STORAGE :: Effect

foreign import basilSet
  :: forall eff.
     String
  -> String
  -> Eff (storage :: STORAGE | eff) Unit

foreign import basilGet
  :: forall eff.
     String
  -> Eff (storage :: STORAGE | eff) (Nullable String)

foreign import basilRemove
  :: forall eff.
     String
  -> Eff (storage :: STORAGE | eff) Unit
