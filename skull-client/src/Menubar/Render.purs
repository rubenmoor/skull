module Menubar.Render
  ( render
  ) where

import Halogen.HTML.Events as Events
import Data.Maybe (Maybe(..))
import Halogen (ComponentHTML)
import Halogen.HTML (button, text)
import Menubar.Types (Query(..), State)
import Prelude (($), (<>))
import Util.HTML (cl, cldiv_, clspan_)

render :: State
       -> ComponentHTML Query
render st =
  cldiv_ "bgdark p1 clearfix"
    [ cldiv_ "left"
        [ clspan_ "bold" [ text "SKULL" ]
        ]
    , cldiv_ "right" $ case st of
        Just userName ->
          [ text $ "Logged in as "
          , clspan_ "bold" $
              [ text userName
              ]
          , text $  ". ("
          , button
              [ cl "button--pure link-light"
              , Events.onClick (Events.input_ Logout)
              ]
              [ text "log out"
              ]
          , text ")"
          ]
        Nothing ->
          [ button
              [ cl "button"
              , Events.onClick (Events.input_ GotoSignupForm)
              ]
              [ text "Sign up"
              ]
          , text " or "
          , button
              [ cl "button--pure link-light"
              , Events.onClick (Events.input_ GotoLoginForm)
              ]
              [ text "log in"
              ]
          ] -- todo: links
    ]
