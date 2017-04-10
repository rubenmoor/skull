module ErrorMessage where

import ErrorMessage.Types
import Halogen.HTML.Events as Events
import Control.Monad.Aff (Aff)
import Data.Lens ((.=))
import Data.Maybe (Maybe(..), maybe)
import ErrorMessage.Render (render)
import Halogen (Component, ComponentDSL, component, put)
import Halogen.HTML (HTML)
import Prelude (type (~>), bind, pure, ($), (<<<))

errorMessage :: forall eff.
                Component HTML Query Input Message (Aff eff)
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
        Query ~> ComponentDSL State Query Message (Aff eff)
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
  Show msg next -> do
    put $ ShowMessage
      { errorMessage: msg
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
