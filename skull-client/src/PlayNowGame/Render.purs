module PlayNowGame.Render
  ( render
  ) where

import Control.Applicative (pure)
import Control.Bind (bind)
import DOM.HTML.HTMLProgressElement (position)
import Data.Array (findIndex, length, replicate, slice, (!!), null, (:))
import Data.Function (($))
import Data.Functor (mapFlipped)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Ring ((-))
import Data.Semiring ((+))
import Data.Show (show)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import Game (handSize)
import Game.Types (Card(..), Kind(HumanPlayNow), Phase(Reveal, Bet, CardOrBet, FirstCard), Player, Victory(None, One), aHand, aStack, gPhase, gPlayers, gRound, hHasSkull, hNumPlains, plAgent, plAlive, plKind, plVictory, stCards)
import Halogen.Component (ComponentHTML)
import Halogen.HTML.Events as Events
import Halogen.HTML.Extended (HTML, button, cl, cldiv_, clsection_, clspan_, div_, faIcon_, img, text)
import Halogen.HTML.Properties (class_, src, width)
import PlayNowGame.Types (Query(..), State, game)
import Types (UrlRoot)

render
  :: forall eff.
     UrlRoot
  -> State
  -> ComponentHTML Query
render urlRoot st =
  cldiv_ "bgwhite p1"
    [ cldiv_ ""
        [ text $ show $ st ^. game ^. gRound
        ]
    , cldiv_ ""
        [ case st ^. game ^. gPhase of
            FirstCard -> text "First card: all players put down one card"
            CardOrBet -> text "Either put down a card, or initiate the betting"
            Bet -> text "Either bet a higher number or pass"
            Reveal -> text "The highest bidder reveals the required number of cards"
        ]
    , clsection_ "container" $ case createSeating $ st ^. game ^. gPlayers of
            Just seating ->
              let Tuple meNumber me = seating.sitMe
                  bots = mapFlipped seating.sitOthers $ \(Tuple i player) ->
                    cldiv_ ("col col-4 mx1 player"
                        <> if player ^. plAlive then "" else "inactive")
                      [ cldiv_ "bglight p1"
                          [ cldiv_ "h2" $
                              [ clspan_ "pr1"
                                  [ faIcon_ "robot"
                                  ]
                              , text $ "Player " <> show (i + 1)
                              ] <> victoryTrophy player
                        , imgHand urlRoot player
                        , cldiv_ "h2" $
                            [ clspan_ "pr1"
                                [ faIcon_ "hand-peace"
                                ]
                            ]
                        ]
                      , div_
                          [ imgStack urlRoot player
                          ]
                      ]
              in  [ cldiv_ "clearfix" bots
                  , cldiv_ "clearfix center"
                      [ imgStack urlRoot me
                      ]
                  , cldiv_ "clearfix center"
                      [ cldiv_ "container"
                          [ cldiv_ "bglight p1"
                              [
                                cldiv_ "h2" $
                                  [ clspan_ "pr1"
                                      [ faIcon_ "smile-o"
                                      ]
                                  , text $ "Player " <> show (meNumber + 1)
                                  ] <> victoryTrophy me
                              , myHand urlRoot (st ^. game ^. gPhase) me
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
        if pl ^. plAgent ^. aHand ^. hHasSkull
            then [ renderCard ImgSkull ]
            else []
      n = pl ^. plAgent ^. aHand ^. hNumPlains
      plains = replicate n $ renderCard ImgPlain
  in  clsection_ "container--small"
        [ cldiv_ "clearfix" $ skull <> plains
        ]

type SubmitMoveAction = Maybe Int

-- img hand and stack

imgStack :: forall p i. UrlRoot -> Player -> HTML p i
imgStack urlRoot player =
    let cards = player ^. plAgent ^. aStack ^. stCards
    in  if null cards
        then img
               [ src $ urlRoot <> "img/card-stack-0.svg"
               , width 60
               ]
        else cldiv_ "card-stack-container" $ imgStack' $ length cards
  where
    imgStack' :: Int -> Array (HTML p i)
    imgStack' 1 = [ imgCardBack 1 ]
    imgStack' n = imgStack' (n - 1) <> [imgCardBack n]

    imgCardBack i = img
      [ src $ urlRoot <> "img/card-back.svg"
      , width 60
      , cl $ "card-stack-" <> show i
      ]

imgHand :: forall p i. UrlRoot -> Player -> HTML p i
imgHand urlRoot player =
  let n = show $ handSize $ player ^. plAgent ^. aHand
  in  img
        [ src $ urlRoot <> "img/card-hand-" <> n <> ".svg"
        ]
