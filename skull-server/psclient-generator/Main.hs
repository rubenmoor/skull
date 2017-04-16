{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens                        ((&), (.~))
import           Data.Proxy
import qualified Data.Set                            as Set
import           Language.PureScript.Bridge          (BridgePart, SumType,
                                                      buildBridge,
                                                      defaultBridge, mkSumType,
                                                      writePSTypes)
import           Language.PureScript.Bridge.TypeInfo (Language (Haskell))
import           Servant.PureScript                  (HasBridge (..),
                                                      defaultSettings,
                                                      readerParams,
                                                      writeAPIModuleWithSettings)

import           Api.Types
import           HttpApp.Api                         (Routes)
import           HttpApp.BotKey.Types                (BotKey)

types :: [SumType 'Haskell]
types =
  [ mkSumType (Proxy :: Proxy UserNewRequest)
  , mkSumType (Proxy :: Proxy UserNewResponse)
  , mkSumType (Proxy :: Proxy LoginRequest)
  , mkSumType (Proxy :: Proxy LoginResponse)
  , mkSumType (Proxy :: Proxy BotKeyNewRequest)
  , mkSumType (Proxy :: Proxy BotKey)
  ]

bridge :: BridgePart
bridge = defaultBridge

data Bridge

instance HasBridge Bridge where
  languageBridge _ = buildBridge bridge

main :: IO ()
main = do
  let outDir = "../skull-client/src"
      settings =
        defaultSettings & readerParams .~ Set.fromList
          [ "AuthToken"
          , "baseURL"
          ]
  writeAPIModuleWithSettings settings outDir (Proxy :: Proxy Bridge) (Proxy :: Proxy Routes)
  writePSTypes outDir  (buildBridge bridge) types
