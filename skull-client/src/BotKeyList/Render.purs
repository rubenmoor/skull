module BotKeyList.Render
  ( render
  ) where

import BotKey.Types as BotKey
import Data.Array as Array
import Halogen.HTML.Events as Events
import BotKey (botKey)
import BotKeyList.Types (Effects, Query(..), Slot, State, botKeys, isLoading)
import Data.Function (flip, ($))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens ((^.))
import Data.List (null)
import Halogen.Component (ParentHTML)
import Halogen.HTML.Extended (button, cl, cldiv_, clspan_, slot, table, tbody_, td_, text, thead)
import Ulff (Ulff)

render
  :: forall eff.
     State
  -> ParentHTML Query BotKey.Query Slot (Ulff (Effects eff))
render st = cldiv_ "bgwhite p1 mx-auto col-8"
  [ button
      [ cl "button"
      , Events.onClick (Events.input_ CreateNew)
      ]
      [ text "Create New"
      ]
  , if st ^. isLoading
    then clspan_ "italic" [ text "Loading ..." ]
    else if null $ st ^. botKeys
         then cldiv_ "italic" [ text "You don't have any botkeys"]
         else table
                [ cl "table"
                ]
                [ thead
                    [ cl "bold"
                    ]
                    [ td_
                        [ text "Label"
                        ]
                    , td_
                        [ text "Key"
                        ]
                    ]
                , tbody_ $
                    let bks = Array.fromFoldable $ st ^. botKeys
                        forIndex = flip mapWithIndex
                    in  forIndex bks $ \i bk ->
                          slot i botKey bk (Events.input Delete)
                ]
  ]
