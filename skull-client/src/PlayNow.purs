module PlayNow
  ( playNow
  ) where

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
import HttpApp.PlayNow.Api.Types (PNActiveResp(..), PNDeleteRq(..), PNNewRq(..), activeGame, activePlayerKey, nrespGame, nrespPlayerKey)
import PlayNow.Render (render)
import PlayNow.Types (Effects, Input, Message, Query(..), Slot, State, initial)
import PlayNowGame.Types (game)
import PlayNowGame.Types as PlayNowGame
import ServerAPI (deletePlayNow, getPlayNowActive, postPlayNowNew)
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
     Query ~> ParentDSL State Query PlayNowGame.Query Slot Message (Ulff (Effects eff))
eval = case _ of
  Initialize next -> do
    mkRequest getPlayNowActive $ \(PNActiveResp mActive) ->
      case mActive of
        Nothing -> put Nothing
        Just active ->
          let pngState =
                { _game: active ^. activeGame
                , _humanPlayerKey: active ^. activePlayerKey
                }
          in  put $ Just pngState
    pure next
  NewGame n next -> do
    let body = PNNewRq
          { _nrqNumPlayers: n
          }
    mkRequest (postPlayNowNew body) $ \resp ->
      put $ Just
        { _game: resp ^. nrespGame
        , _humanPlayerKey: resp ^. nrespPlayerKey
        }
    pure next
  HandleMsg PlayNowGame.Delete next -> do
    mActive <- get
    for_ mActive $ \active -> do
      let body = PNDeleteRq
            { _drqKey: active ^. game ^. gKey
            }
      mkRequest (deletePlayNow body) $ \_ -> put Nothing
    pure next
