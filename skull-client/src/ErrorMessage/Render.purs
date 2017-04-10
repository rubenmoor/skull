module ErrorMessage.Render where

import ErrorMessage.Types
import Data.Array ((:))
import Halogen (ComponentHTML)
import Halogen.HTML (div_, text)
import Prelude (pure, unit, ($), (<<<), (<>))
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
      , text msg.errorMessage.title
      , faButton_ "times" Dismiss
      ]
      <> if msg.showDetails
            then [ div_
                     [ text msg.errorMessage.details
                     ]
                 ]
            else []
