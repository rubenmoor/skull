{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative                 ((<|>))
import           Control.Lens                        ((&), (.~))
import           Data.Proxy
import qualified Data.Set                            as Set
import           Language.PureScript.Bridge          (BridgePart, SumType,
                                                      buildBridge,
                                                      defaultBridge, mkSumType,
                                                      typeName, writePSTypes,
                                                      (^==))
import           Language.PureScript.Bridge.PSTypes  (psString)
import           Language.PureScript.Bridge.TypeInfo (Language (Haskell))
import           Servant.PureScript                  (HasBridge (..),
                                                      defaultSettings,
                                                      readerParams,
                                                      writeAPIModuleWithSettings)

import           Api.Types
import           Auth.Types                          (AuthToken)
import           Game.Types                          (BetState, Card, GState,
                                                      Hand, Info, Kind, Phase,
                                                      Player, Stack, Victory,
                                                      VictoryInfo, VictoryType)
import           HttpApp.Api                         (Routes)
import           HttpApp.BotKey.Types                (BotKey)

types :: [SumType 'Haskell]
types =
  [ mkSumType (Proxy :: Proxy AuthToken)
  , mkSumType (Proxy :: Proxy UserNewRq)
  , mkSumType (Proxy :: Proxy UserNewResp)
  , mkSumType (Proxy :: Proxy UserNameResp)
  , mkSumType (Proxy :: Proxy UserExistsRq)
  , mkSumType (Proxy :: Proxy LoginRq)
  , mkSumType (Proxy :: Proxy LoginResp)
  , mkSumType (Proxy :: Proxy LogoutResp)
  , mkSumType (Proxy :: Proxy BKNewResp)
  , mkSumType (Proxy :: Proxy BKAllResp)
  , mkSumType (Proxy :: Proxy BKSetLabelRq)
  , mkSumType (Proxy :: Proxy BKSetLabelResp)
  , mkSumType (Proxy :: Proxy BKDeleteRq)
  , mkSumType (Proxy :: Proxy BotKey)
  , mkSumType (Proxy :: Proxy PNNewRq)
  , mkSumType (Proxy :: Proxy PNNewResp)
  , mkSumType (Proxy :: Proxy PNAllResp)
  , mkSumType (Proxy :: Proxy PNDeleteRq)
  , mkSumType (Proxy :: Proxy Info)
  , mkSumType (Proxy :: Proxy GState)
  , mkSumType (Proxy :: Proxy VictoryInfo)
  , mkSumType (Proxy :: Proxy VictoryType)
  , mkSumType (Proxy :: Proxy Phase)
  , mkSumType (Proxy :: Proxy Player)
  , mkSumType (Proxy :: Proxy Victory)
  , mkSumType (Proxy :: Proxy Kind)
  , mkSumType (Proxy :: Proxy Hand)
  , mkSumType (Proxy :: Proxy Stack)
  , mkSumType (Proxy :: Proxy Card)
  , mkSumType (Proxy :: Proxy BetState)
  ]

base64Bridge :: BridgePart
base64Bridge = typeName ^== "Base64" >> pure psString

bridge :: BridgePart
bridge = defaultBridge <|> base64Bridge

data Bridge

instance HasBridge Bridge where
  languageBridge _ = buildBridge bridge

main :: IO ()
main = do
  let outDir = "../skull-client/src"
      settings =
        defaultSettings & readerParams .~
          [ "AuthToken"
          , "baseURL"
          ]
  writeAPIModuleWithSettings settings outDir (Proxy :: Proxy Bridge) (Proxy :: Proxy Routes)
  writePSTypes outDir  (buildBridge bridge) types
