module Menubar.Render
  ( render
  ) where

import Halogen.HTML.Events as Events
import Data.Maybe (Maybe(..))
import Halogen (ComponentHTML)
import Halogen.HTML (button, div_, text)
import Menubar.Types (Query(..), State)
import Prelude (($), (<>))
import Types (Env)
import Util.HTML (cldiv_)

render :: Env
       -> State
       -> ComponentHTML Query
render env st =
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
        Nothing -> [ text "Sign up" ] -- todo: links
    ]
