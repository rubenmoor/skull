module Menubar
  ( menubar
  ) where

import Data.Unit (unit)
import Halogen.HTML.Events as Events
import Control.Monad.State (put)
import Halogen (Component, ComponentDSL, component)
import Halogen.HTML (HTML)
import Menubar.Render (render)
import Menubar.Types (Input, Message, Query(..), State, Effects, initial)
import Prelude (type (~>), discard, pure, ($))
import Router (AuthFormLocation(..), Location(..), LoggedOutLocation(..), gotoLocation)
import ServerAPI (getUserLogout)
import Ulff (Ulff, mkRequest)

menubar :: forall eff.
           Component HTML Query Input Message (Ulff (Effects eff))
menubar =
  component
    { initialState: initial
    , render: render
    , eval: eval
    , receiver: Events.input HandleInput
    }

eval :: forall eff.
        Query ~> ComponentDSL State Query Message (Ulff (Effects eff))
eval = case _ of
  HandleInput mUserName next -> do
    put mUserName
    pure next
  Logout next -> do
    mkRequest getUserLogout (\_ -> pure unit)
    pure next
  GotoSignupForm next -> do
    gotoLocation $ LocLoggedOut $ LocAuthForms LocSignupForm
    pure next
  GotoLoginForm next -> do
    gotoLocation $ LocLoggedOut $ LocAuthForms LocLoginForm
    pure next
