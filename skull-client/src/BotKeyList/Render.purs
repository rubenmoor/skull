module BotKeyList.Render
  ( render
  ) where

import BotKey.Types as BotKey
import Data.Array as Array
import Halogen.HTML.Events as Events
import BotKey (botKey)
import BotKeyList.Types (Effects, Query(..), Slot, State, botKeys, isLoading)
import Data.Function (($))
import Data.Functor (mapFlipped)
import Data.Lens ((^.))
import Data.List (List(..), null, (:))
import Data.Semiring ((+))
import Data.Tuple (Tuple(..))
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
                , tbody_
                    let bks = enumerate $ st ^. botKeys
                    in  Array.fromFoldable $ mapFlipped bks $ \(Tuple i bk) ->
                          slot i botKey bk (Events.input Delete)
                ]
  ]

enumerate :: forall a. List a -> List (Tuple Int a)
enumerate = enumerate' 0
  where
    enumerate' :: Int -> List a -> List (Tuple Int a)
    enumerate' _ Nil = Nil
    enumerate' n (x:xs)  = Tuple n x : enumerate' (n + 1) xs
