module PlayNowGame
  ( playNowGame
  ) where

import Control.Applicative (pure)
import Control.Bind (discard, bind)
import Control.Monad.State (get, put)
import Data.Function (($), (<<<))
import Data.Functor (($>))
import Data.Lens ((.=), (^.))
import Data.NaturalTransformation (type (~>))
import Game.Api.Types (AuthInfo(..), ErrorOr(..), GameError(..))
import Game.Play.Api.Types (PlayCardRq(..))
import Game.Types (GState(..), gKey, gState)
import Halogen (Component)
import Halogen.Component (ComponentDSL, component)
import Halogen.HTML (HTML)
import Halogen.HTML.Events as Events
import Halogen.Query (raise)
import HttpApp.PlayNow.Api.Types (PNDeleteRq(..))
import PlayNowGame.Render (render)
import PlayNowGame.Types (Effects, Input, Message(..), Query(HandleInput, PlayCard, AbortGame), State, game, humanPlayerKey, initial)
import ServerAPI (deletePlayNow, postGamePlayCard)
import Types (UrlRoot)
import Ulff (Ulff, mkRequest)

playNowGame
  :: forall eff.
     UrlRoot
  -> Component HTML Query Input Message (Ulff (Effects eff))
playNowGame urlRoot =
  component
    { initialState: initial
    , render: render urlRoot
    , eval
    , receiver: Events.input HandleInput
    }

getAuthInfo
  :: State
  -> AuthInfo
getAuthInfo st = AuthInfo
  { _aiGameKey: st ^. game ^. gKey
  , _aiPlayerKey: st ^. humanPlayerKey
  }

eval
  :: forall eff.
     Query ~> ComponentDSL State Query Message (Ulff (Effects eff))
eval = case _ of
  HandleInput st next -> put st $> next
  PlayCard c next -> do
    st <- get
    let body = PlayCardRq
          { _pcrqAuth: getAuthInfo st
          , _pcrqCard: c
          }
    mkRequest (postGamePlayCard body) $ case _ of
      Error e ->
        let (GameError r) = e.geMsg
        in  game <<< gState .= Aborted r.unGameError
      Result g -> game .= g
    pure next
  AbortGame next -> do
    st <- get
    let body = PNDeleteRq
          { _drqKey: st ^. game ^. gKey
          }
    mkRequest (deletePlayNow body) $ \_ -> raise Delete
    pure next
