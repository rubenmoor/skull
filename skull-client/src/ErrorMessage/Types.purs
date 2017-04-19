module ErrorMessage.Types where

import Data.Lens (Lens', Prism', lens, prism')
import Data.Maybe (Maybe(..))
import Data.Void (Void)
import Prelude (($))
import Types (Error)

-- Input

type Input = Maybe Error

-- State

data State
  = NoErrorMessage
  | ShowMessage ShowMessage

type ShowMessage =
  { errorMessage :: Error
  , showDetails :: Boolean
  }

initial :: State
initial = NoErrorMessage

_ShowMessage :: Prism' State ShowMessage
_ShowMessage = prism' ShowMessage $ case _ of
  NoErrorMessage -> Nothing
  ShowMessage msg -> Just msg

_errorMessage :: Lens' ShowMessage Error
_errorMessage = lens _.errorMessage (\r msg -> r { errorMessage = msg })

_showDetails :: Lens' ShowMessage Boolean
_showDetails = lens _.showDetails (\r b -> r { showDetails = b })

-- Query

data Query a
  = HandleInput Input a
  | Show Error a
  | ShowDetails a
  | HideDetails a
  | Dismiss a

-- Message

type Message = Void
