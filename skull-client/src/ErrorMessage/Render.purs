module ErrorMessage.Render where

import ErrorMessage.Types
import Data.Lens ((^.))
import Halogen (ComponentHTML)
import Halogen.HTML (div_, text)
import Prelude (($), (<>))
import Types (_details, _title)
import Util.HTML (cldiv_, faButton_)

render :: State
       -> ComponentHTML Query
render = case _ of
  NoErrorMessage  -> cldiv_ "errorMessage empty" []
  ShowMessage msg -> do
    cldiv_ "errorMessage" $
      [ if msg.showDetails
           then faButton_ "chevron-down"  HideDetails
           else faButton_ "chevron-right" ShowDetails
      , text $ msg.errorMessage ^. _title
      , faButton_ "times" Dismiss
      ]
      <> if msg.showDetails
            then [ div_
                     [ text $ msg.errorMessage ^. _details
                     ]
                 ]
            else []
