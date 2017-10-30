module PlayNow
  ( playNow
  ) where

import BotKey.Types as BotKey
import Control.Applicative (pure)
import Control.Bind (discard, bind)
import Control.Monad.State (get, put)
import Data.Foldable (for_)
import Data.Function (const, ($))
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Data.NaturalTransformation (type (~>))
import Game.Types (gKey)
import Halogen (Component, action, lifecycleParentComponent)
import Halogen.Component (ParentDSL)
import Halogen.HTML (HTML)
import HttpApp.PlayNow.Api.Types (PNDeleteRq(..), PNNewRq(..), arespGame, nrespGame)
import PlayNow.Render (render)
import PlayNow.Types (Effects, Input, Query(..), Slot, State, Message, initial)
import ServerAPI (deletePlayNow, getPlayNowAll, postPlayNowNew)
import Types (UrlRoot)
import Ulff (Ulff, mkRequest)

playNow
  :: forall eff.
     UrlRoot
  -> Component HTML Query Input Message (Ulff (Effects eff))
playNow urlRoot =
  lifecycleParentComponent
    { initialState: initial
    , render: render urlRoot
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
      put $ resp ^. arespGame
    pure next
  NewGame n next -> do
    let body = PNNewRq
          { _nrqNumPlayers: n
          }
    mkRequest (postPlayNowNew body) $ \resp ->
      put $ Just $ resp ^. nrespGame
    pure next
  AbortGame next -> do
    mInfo <- get
    for_ mInfo $ \info -> do
      let body = PNDeleteRq
            { _drqKey: info ^. gKey
            }
      mkRequest (deletePlayNow body) $ \_ -> put Nothing
    pure next
