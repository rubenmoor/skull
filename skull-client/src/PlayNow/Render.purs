module PlayNow.Render
  ( render
  ) where

import BotKey.Types as BotKey
import Halogen.HTML.Events as Events
import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Array (findIndex, length, replicate, slice, (!!))
import Data.Function (($))
import Data.Functor (mapFlipped)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Semiring ((+))
import Data.Show (show)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import Game (handSize)
import Game.Types (Card(..), Kind(HumanPlayNow), Phase(Reveal, Bet, CardOrBet, FirstCard), Player, Victory(None, One), gPhase, gPlayers, gRound, hHasSkull, hNumPlains, plAlive, plHand, plKind, plStack, plVictory, stCards)
import Halogen.Component (ParentHTML)
import Halogen.HTML.Extended (button, cl, cldiv_, clsection_, clspan_, div_, faIcon_, img, text, HTML)
import Halogen.HTML.Properties (src)
import PlayNow.Types (Effects, Query(..), Slot, State)
import Types (UrlRoot)
import Ulff (Ulff)

render
  :: forall eff.
     UrlRoot
  -> State
  -> ParentHTML Query BotKey.Query Slot (Ulff (Effects eff))
render urlRoot = case _ of
  Nothing -> cldiv_ "bgwhite p1"
    [ button
        [ cl "button"
        , Events.onClick $ Events.input_ (NewGame 4)
        ]
        [ text "New Game"
        ]
    ]
  Just game -> cldiv_ "bgwhite p1"
    [ cldiv_ ""
        [ text $ show $ game ^. gRound
        ]
    , cldiv_ ""
        [ case game ^. gPhase of
            FirstCard -> text "First card: all players put down one card"
            CardOrBet -> text "Either put down a card, or initiate the betting"
            Bet -> text "Either bet a higher number or pass"
            Reveal -> text "The highest bidder reveals the required number of cards"
        ]
    , clsection_ "container" $ case createSeating $ game ^. gPlayers of
            Just seating ->
              let Tuple meNumber me = seating.sitMe
                  bots = mapFlipped seating.sitOthers $ \(Tuple i player) ->
                    cldiv_ ("col col-4 mx1 player"
                        <> if player ^. plAlive then "" else "inactive")
                      [ cldiv_ "bglight p1"
                          [ cldiv_ "h2" $
                              [ clspan_ "pr1"
                                  [ faIcon_ "android"
                                  ]
                              , text $ "Player " <> show (i + 1)
                              ] <> victoryTrophy player
                        , imgHand urlRoot player
                        ]
                      , div_
                          [ imgStack urlRoot player
                          ]
                      ]
              in  [ cldiv_ "clearfix" bots
                  , cldiv_ "clearfix center"
                      [ cldiv_ "container"
                          [ div_
                              [ imgStack urlRoot me
                              ]
                          , cldiv_ "bglight p1"
                              [
                                cldiv_ "h2" $
                                  [ clspan_ "pr1"
                                      [ faIcon_ "smile-o"
                                      ]
                                  , text $ "Player " <> show (meNumber + 1)
                                  ] <> victoryTrophy me
                              , myHand urlRoot (game ^. gPhase) me
                              ]
                          ]
                      ]
                  ]
            Nothing -> [ text "Failed to load players"]
    , button
        [ cl "button--pure link-light"
        , Events.onClick $ Events.input_ AbortGame
        ]
        [ text "Abort"
        ]
    ]

-- seating

type Seating =
  { sitOthers :: Array (Tuple Int Player)
  , sitMe :: Tuple Int Player
  }

createSeating :: Array Player -> Maybe Seating
createSeating ps = do
  meI <- findIndex (\p -> case p ^. plKind of
                             HumanPlayNow -> true
                             _ -> false) ps
  let pis = mapWithIndex Tuple ps
  me <- pis !! meI
  let rights = slice 0 meI pis
      lefts = slice (meI + 1) (length pis) pis
  pure
    { sitOthers: lefts <> rights
    , sitMe: me
    }

-- victory trophy

victoryTrophy :: forall p i. Player -> Array (HTML p i)
victoryTrophy player =
  case player ^. plVictory of
    One  -> [ clspan_ "p1 yellow" [ faIcon_ "trophy" ] ]
    None -> []

-- my hand

data ImgCard
  = ImgPlain
  | ImgSkull

myHand
  :: forall t.
     String
  -> Phase
  -> Player
  -> HTML t (Query Unit)
myHand urlRoot phase pl =
  let renderImg imgCard =
        let str = case imgCard of
              ImgPlain -> "plain"
              ImgSkull -> "skull"
        in img
             [ src $ urlRoot <> "img/card-" <> str <> ".svg"
             ]
      renderButton contents = button
        [ cl "button--pure"
        , Events.onClick $ Events.input_ $ PlayCard Skull
        ]
        [ contents
        ]
      renderCard img =
        cldiv_ "col col-3 pl1"
          [ case phase of
               FirstCard -> renderButton $ renderImg img
               CardOrBet -> renderButton $ renderImg img
               _         -> renderImg img
          ]
      skull =
        if pl ^. plHand ^. hHasSkull
            then [ renderCard ImgSkull ]
            else []
      n = pl ^. plHand ^. hNumPlains
      plains = replicate n $ renderCard ImgPlain
  in  clsection_ "container--small"
        [ cldiv_ "clearfix" $ skull <> plains
        ]

type SubmitMoveAction = Maybe Int

-- img hand and stack

imgHand :: forall p i. UrlRoot -> Player -> HTML p i
imgHand urlRoot player =
  imgHandStack urlRoot player ImgHand (\p -> handSize $ p ^. plHand)

imgStack :: forall p i. UrlRoot -> Player -> HTML p i
imgStack urlRoot player =
  imgHandStack urlRoot player ImgStack (\p -> length $ p ^. plStack ^. stCards)

data ImgHandStack
  = ImgHand
  | ImgStack

imgHandStack :: forall p i. UrlRoot -> Player -> ImgHandStack -> (Player -> Int) -> HTML p i
imgHandStack urlRoot player imgHS getNumber =
  let n = show $ getNumber player
      str = case imgHS of
        ImgHand -> "hand"
        ImgStack -> "stack"
  in  img
        [ src $ urlRoot <> "img/card-" <> str <> "-" <> n <> ".svg"
        ]
