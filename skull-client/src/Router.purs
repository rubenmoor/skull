module Router where

import Control.Alternative ((<|>))
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import DOM (DOM)
import Prelude (Unit, (<$), (<$>), (<<<))
import Routing.Hash (setHash)
import Routing.Match (Match)
import Routing.Match.Class (lit)

data Location
  = LocLoggedIn  LoggedInLocation
  | LocLoggedOut LoggedOutLocation

data LoggedInLocation
  = LocLoggedInPublic PublicLocation
  | LocPrivate  PrivateLocation

data LoggedOutLocation
  = LocLoggedOutPublic PublicLocation
  | LocAuthForms AuthFormLocation

data PublicLocation
  = LocHome

data PrivateLocation
  = LocBotKeys

data AuthFormLocation
  = LocSignupForm
  | LocLoginForm

routing :: Match Location
routing =
      LocLoggedOut <$> (    LocLoggedOutPublic <$> (LocHome <$ lit "public")
                        <|> LocAuthForms       <$> (    LocSignupForm <$ lit "signup"
                                                    <|> LocLoginForm  <$ lit "login"
                                                   )
                       )
  <|> LocLoggedIn  <$> (    LocPrivate         <$> (LocBotKeys <$ lit "botKeys")
                        <|> LocLoggedInPublic  <$> (LocHome    <$ lit "private")
                       )

toPath :: Location -> String
toPath = case _ of
  LocLoggedIn loc -> case loc of
    LocLoggedInPublic sLoc -> case sLoc of
      LocHome -> "private"
    LocPrivate        sLoc -> case sLoc of
      LocBotKeys -> "botKeys"
  LocLoggedOut loc -> case loc of
    LocLoggedOutPublic sLoc -> case sLoc of
      LocHome -> "public"
    LocAuthForms       sLoc -> case sLoc of
      LocSignupForm -> "signup"
      LocLoginForm -> "login"

gotoLocation :: forall eff m.
                MonadEff (dom :: DOM | eff) m
             => Location
             -> m Unit
gotoLocation = liftEff <<< setHash <<< toPath
