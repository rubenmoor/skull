{-# LANGUAGE DataKinds #-}

module Main where

import           Data.Proxy
import           Language.PureScript.Bridge          (BridgePart, FullBridge,
                                                      SumType, buildBridge,
                                                      defaultBridge, mkSumType,
                                                      typeName, writePSTypes,
                                                      (<|>), (^==))
import           Language.PureScript.Bridge.TypeInfo (Language (Haskell))
import           Servant.PureScript                  (HasBridge (..),
                                                      writeAPIModule)

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
  writeAPIModule outDir (Proxy :: Proxy Bridge) (Proxy :: Proxy Routes)
  writePSTypes outDir  (buildBridge bridge) types
