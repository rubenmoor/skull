module PlayNow.Render
  ( render
  ) where

import BotKey.Types as BotKey
import Data.Array as Array
import Halogen.HTML.Events as Events
import Data.Function (flip, ($))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Semiring ((+))
import Data.Show (show)
import Game (handSize)
import Game.Types (GState(..), Nature(..), Phase(..), Victory(..), giPhase, giPlayers, giState, plAlive, plHand, plNature, plVictory)
import Halogen.Component (ParentHTML)
import Halogen.HTML (button, text)
import Halogen.HTML.Extended (cl, cldiv_, faIcon_)
import PlayNow.Types (Effects, Query(..), Slot, State)
import Ulff (Ulff)

render
  :: forall eff.
     State
  -> ParentHTML Query BotKey.Query Slot (Ulff (Effects eff))
render = case _ of
  Nothing -> cldiv_ "bgwhite p1"
    [ button
        [ cl "button"
        , Events.onClick $ Events.input_ (NewGame 4)
        ]
        [ text "New Game"
        ]
    ]
  Just info -> cldiv_ "bgwhite p1"
    [ cldiv_ ""
        [ case info ^. giState of
            Round n -> text $ "Round " <> show n
            Finished vInfo -> text "Game over"
            Aborted str -> text $ "Game aborted: " <> str
        ]
    , cldiv_ ""
        [ case info ^. giPhase of
            FirstCard -> text "First card: all players put down one card"
            CardOrBet -> text "Either put down a card, or initiate the betting"
            Bet -> text "Either bet a higher number or pass"
            Reveal -> text "The highest bidder reveals the required number of cards"
        ]
    , cldiv_ "" $
        let ps = Array.fromFoldable $ info ^. giPlayers
            forIndex = flip mapWithIndex
        in  forIndex ps $ \i player ->
              cldiv_ (if player ^. plAlive then "" else "inactive")
                [ cldiv_ "h2" [ text $ "Player " <> show (i + 1)]
                , case player ^. plNature of
                    Bot -> faIcon_ "android"
                    Human -> faIcon_ "smile-o"
                , cldiv_ "h1" case player ^. plVictory of
                     One -> [ faIcon_ "trophy" ]
                     None ->  []
                , cldiv_ "border" [ text $ show $ handSize $ player ^. plHand ]
                ]
    , button
        [ cl "button--pure link-light"
        , Events.onClick $ Events.input_ AbortGame
        ]
        [ text "Abort"
        ]
    ]
