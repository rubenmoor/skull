module ErrorMessage.Types where

import Data.Lens (Lens', Prism', lens, prism')
import Data.Maybe (Maybe(..))
import Data.Void (Void)
import Prelude (($))

-- interface

type ErrorMessage =
  { title :: String
  , details :: String
  }

_title :: Lens' ErrorMessage String
_title = lens _.title (\r str -> r { title = str })

_details :: Lens' ErrorMessage String
_details = lens _.details (\r str -> r { details = str })


-- Input

type Input = Maybe ErrorMessage

-- State

data State
  = NoErrorMessage
  | ShowMessage ShowMessage

type ShowMessage =
  { errorMessage :: ErrorMessage
  , showDetails :: Boolean
  }

initial :: State
initial = NoErrorMessage

_ShowMessage :: Prism' State ShowMessage
_ShowMessage = prism' ShowMessage $ case _ of
  NoErrorMessage -> Nothing
  ShowMessage msg -> Just msg

_errorMessage :: Lens' ShowMessage ErrorMessage
_errorMessage = lens _.errorMessage (\r msg -> r { errorMessage = msg })

_showDetails :: Lens' ShowMessage Boolean
_showDetails = lens _.showDetails (\r b -> r { showDetails = b })

-- Query

data Query a
  = HandleInput Input a
  | Show ErrorMessage a
  | ShowDetails a
  | HideDetails a
  | Dismiss a

-- Message

type Message = Void
