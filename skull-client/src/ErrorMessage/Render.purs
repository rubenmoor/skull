module ErrorMessage.Render where

import ErrorMessage.Types
import Data.Lens ((^.))
import Halogen (ComponentHTML)
import Prelude (($), (<>))
import Types (_details, _title)
import Halogen.HTML.Extended (cldiv_, faButton_, text)

render :: State
       -> ComponentHTML Query
render = case _ of
  NoErrorMessage  -> cldiv_ "errorMessage empty" []
  ShowMessage msg -> cldiv_ "bgred p1" $
    [ cldiv_ "clearfix" $
        [ cldiv_ "left"
            [ if msg.showDetails
                then faButton_ "chevron-down link-white h4"  HideDetails
                else faButton_ "chevron-right link-white h4" ShowDetails
            ]
        , cldiv_ "left"
            [ text $ msg.errorMessage ^. _title
            ]
        , cldiv_ "right"
            [ faButton_ "times link-white h4 right" Dismiss
            ]
        ]
    ] <> if msg.showDetails
            then [ cldiv_ "h6 p1 border"
                     [ text $ msg.errorMessage ^. _details
                     ]
                 ]
            else []
