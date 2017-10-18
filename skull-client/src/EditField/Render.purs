module EditField.Render
  ( render
  ) where

import Halogen.HTML.Events as Events
import DOM.Event.KeyboardEvent (key)
import Data.Function (($))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.String (null)
import Data.Tuple (Tuple(..))
import EditField.Types (Query(..), State)
import Halogen.Component (ComponentHTML)
import Halogen.HTML.Extended (cl, input, span, text)
import Halogen.HTML.Properties (autofocus, value)
import Halogen.Query (action)

render
  :: State
  -> ComponentHTML Query
render st =
  if st.editing
     then input
            [ value st.newLabel
            , autofocus true
            , Events.onValueInput $ Events.input SetLabel
            , Events.onBlur $ Events.input_ CancelEditLabel
            , Events.onKeyDown \e -> case key e of
                "Enter" -> Just $ action SubmitLabel
                "Escape" -> Just $ action CancelEditLabel
                x -> Nothing
            ]
     else
       let (Tuple cltyp label) =
             if null st.label
                then Tuple "italic" "unnamed"
                else Tuple "bold" st.label
       in  span
             [ cl $ "editField " <> cltyp
             , Events.onClick $ Events.input_ StartEditLabel
             ]
             [ text label ]
