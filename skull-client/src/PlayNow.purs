module PlayNow
  ( playNow
  ) where

import BotKey.Types as BotKey
import Control.Applicative (pure)
import Control.Bind (discard)
import Control.Monad.State (put)
import Data.Function (const, ($))
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Data.NaturalTransformation (type (~>))
import Halogen (Component, action, lifecycleParentComponent)
import Halogen.Component (ParentDSL)
import Halogen.HTML (HTML)
import HttpApp.PlayNow.Api.Types (PNNewRq(..), arespInfo, nrespInfo)
import PlayNow.Render (render)
import PlayNow.Types (Effects, Input, Query(..), Slot, State, Message, initial)
import ServerAPI (getPlayNowAll, postPlayNowNew)
import Ulff (Ulff, mkRequest)

playNow
  :: forall eff.
     Component HTML Query Input Message (Ulff (Effects eff))
playNow =
  lifecycleParentComponent
    { initialState: initial
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ action Initialize
    , finalizer: Nothing
    }

eval
  :: forall eff.
     Query ~> ParentDSL State Query BotKey.Query Slot Message (Ulff (Effects eff))
eval = case _ of
  Initialize next -> do
    mkRequest getPlayNowAll $ \resp ->
      put $ resp ^. arespInfo
    pure next
  NewGame n next -> do
    let body = PNNewRq
          { _nrqNumPlayers: n
          }
    mkRequest (postPlayNowNew body) $ \resp ->
      put $ Just $ resp ^. nrespInfo
    pure next
  AbortGame next -> do
    put Nothing
    pure next
