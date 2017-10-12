module BotKeyList.Render
  ( render
  ) where

import Data.Array as Array
import BotKey (botKey)
import BotKeyList.Types (ChildQuery, ChildSlot, Query, State, Effects)
import Data.Function (($))
import Data.Functor (mapFlipped)
import Data.List (List(..), (:))
import Data.Semiring ((+))
import Data.Tuple (Tuple(..))
import Data.Void (absurd)
import Halogen.Component (ParentHTML)
import Halogen.Component.ChildPath (cp1)
import Halogen.HTML (h1_, slot', table_, tbody_, text)
import Ulff (Ulff)
import Util.HTML (cldiv_)

render
  :: forall eff.
     State
  -> ParentHTML Query ChildQuery ChildSlot (Ulff (Effects eff))
render st = cldiv_ "p1"
  [ h1_
      [ text "BotKeys"
      ]
  , table_
      [ tbody_
          let bks = enumerate st.botKeys
          in  Array.fromFoldable $ mapFlipped bks $ \(Tuple i bk) ->
                slot' cp1 i botKey bk absurd
      ]
  ]

enumerate :: forall a. List a -> List (Tuple Int a)
enumerate = enumerate' 0
  where
    enumerate' :: Int -> List a -> List (Tuple Int a)
    enumerate' _ Nil = Nil
    enumerate' n (x:xs)  = Tuple n x : enumerate' (n + 1) xs
