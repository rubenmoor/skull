module ErrorMessage where

import ErrorMessage.Types
import Halogen.HTML.Events as Events
import Control.Applicative (pure)
import Control.Bind (discard)
import Control.Category ((<<<))
import Data.Function (($))
import Data.Lens ((.=))
import Data.Maybe (Maybe(..), maybe)
import Data.NaturalTransformation (type (~>))
import ErrorMessage.Render (render)
import Halogen (Component, ComponentDSL, component, put)
import Halogen.HTML (HTML)
import Ulff (Ulff)

errorMessage :: forall eff.
                Component HTML Query Input Message (Ulff eff)
errorMessage =
  component
    { initialState: initialize
    , render
    , eval
    , receiver: Events.input HandleInput
    }

initialize :: Input -> State
initialize =
  maybe initial $ \msg ->
    ShowMessage { errorMessage : msg, showDetails : false }

eval :: forall eff.
        Query ~> ComponentDSL State Query Message (Ulff eff)
eval = case _ of
  HandleInput mMsg next ->
    case mMsg of
      Nothing -> pure next
      Just msg -> do
        put $ ShowMessage
          { errorMessage: msg
          , showDetails: false
          }
        pure next
  Show err next -> do
    put $ ShowMessage
      { errorMessage: err
      , showDetails: false
      }
    pure next
  ShowDetails next -> do
    _ShowMessage <<< _showDetails .= true
    pure next
  HideDetails next -> do
    _ShowMessage <<< _showDetails .= false
    pure next
  Dismiss next -> do
    put NoErrorMessage
    pure next
