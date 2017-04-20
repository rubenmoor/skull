module Router where

import Control.Alternative ((<|>))
import Prelude ((<$), (<$>))
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
      LocLoggedIn  <$> (    LocLoggedInPublic  <$> (LocHome    <$ lit "public")
                        <|> LocPrivate         <$> (LocBotKeys <$ lit "botKeys")
                       )
  <|> LocLoggedOut <$> (    LocLoggedOutPublic <$> (LocHome <$ lit "")
                        <|> LocAuthForms       <$> (    LocSignupForm <$ lit "signup"
                                                    <|> LocLoginForm  <$ lit "login"
                                                   )
                       )
