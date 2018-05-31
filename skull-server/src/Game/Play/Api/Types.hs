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
import           Game.Types      (Card (..))

data PlayCardRq = PlayCardRq
  { _pcrqCard :: Card
  , _pcrqAuth :: AuthInfo
  } deriving (Generic, FromJSON, ToJSON)

instance ToSample PlayCardRq where
  toSamples _ =
    let _pcrqCard = Plain
        _pcrqAuth = sampleAuthInfo
    in  singleSample PlayCardRq{..}

makeLenses ''PlayCardRq
