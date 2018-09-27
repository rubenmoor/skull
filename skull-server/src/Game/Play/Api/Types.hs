{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- for the client: export those types needed for playnow
module Game.Play.Api.Types where

import           Control.Lens.TH (makeLenses)
import           Data.Aeson      (FromJSON, ToJSON)
import           GHC.Generics    (Generic)

import           Servant.Docs    (ToSample (..), singleSample)

import           Game.Api.Types  (AuthInfo, sampleAuthInfo)
import           Game.Types      (CardKind (..))

data PlayCardRq = PlayCardRq
  { _pcrqCardKind :: CardKind
  , _pcrqAuth     :: AuthInfo
  } deriving (Generic, FromJSON, ToJSON)

instance ToSample PlayCardRq where
  toSamples _ =
    let _pcrqCardKind = Plain
        _pcrqAuth = sampleAuthInfo
    in  singleSample PlayCardRq{..}

data PlaceBetRq = PlaceBetRq
  { _pbrqValue :: Int
  , _pbrqAuth  :: AuthInfo
  } deriving (Generic, FromJSON, ToJSON)

instance ToSample PlaceBetRq where
  toSamples _ =
    let _pbrqValue = 3
        _pbrqAuth = sampleAuthInfo
    in  singleSample PlaceBetRq{..}

makeLenses ''PlayCardRq
makeLenses ''PlaceBetRq
