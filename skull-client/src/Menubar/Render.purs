module Menubar.Render
  ( render
  ) where

import Halogen.HTML.Events as Events
import Data.Maybe (Maybe(..))
import Halogen (ComponentHTML)
import Halogen.HTML (button, div_, text)
import Menubar.Types (Query(..), State)
import Prelude (($), (<>))
import Util.HTML (cldiv_)

render :: State
       -> ComponentHTML Query
render st =
  cldiv_ "menubar"
    [  div_ $ case st of
        Just userName ->
          [ text $ "Logged in as " <> userName <> ". ("
          , button
              [ Events.onClick (Events.input_ Logout)
              ]
              [ text "log out"
              ]
          , text ")"
          ]
        Nothing ->
          [ button
              [ Events.onClick (Events.input_ GotoSignupForm)
              ]
              [ text "Sign up"
              ]
          , text " or "
          , button
              [ Events.onClick (Events.input_ GotoLoginForm)
              ]
              [ text "log in"
              ]
          ] -- todo: links
    ]
