module Menubar
  ( menubar
  ) where

import Halogen.HTML.Events as Events
import Basil (clearSessionKey)
import Control.Monad.State (put)
import Halogen (Component, ComponentDSL, component)
import Halogen.HTML (HTML)
import HttpApp.User.Api.Types (LogoutResponse(..))
import Menubar.Render (render)
import Menubar.Types (Input, Message, Query(..), State, Effects, initial)
import Prelude (type (~>), discard, pure, ($))
import Router (AuthFormLocation(LocLoginForm, LocSignupForm), Location(LocLoggedOut), LoggedOutLocation(LocAuthForms, LocLoggedOutPublic), PublicLocation(LocHome), gotoLocation)
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
    mkRequest getUserLogout $ \LogoutResponse -> do
      clearSessionKey
      gotoLocation $ LocLoggedOut $ LocLoggedOutPublic LocHome
    pure next
  GotoSignupForm next -> do
    gotoLocation $ LocLoggedOut $ LocAuthForms LocSignupForm
    pure next
  GotoLoginForm next -> do
    gotoLocation $ LocLoggedOut $ LocAuthForms LocLoginForm
    pure next
