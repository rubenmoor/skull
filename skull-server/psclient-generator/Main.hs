{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Prelude                                   hiding (lines,
                                                            readFile, unlines,
                                                            writeFile)

import           Control.Applicative                       ((<|>))
import           Control.Lens                              ((&), (.~))
import           Data.Proxy
import           Data.Text                                 (Text, lines,
                                                            unlines)
import           Data.Text.IO                              (readFile, writeFile)
import           Language.PureScript.Bridge                (BridgePart, SumType,
                                                            buildBridge,
                                                            defaultBridge,
                                                            mkSumType, typeName,
                                                            writePSTypes, (^==))
import           Language.PureScript.Bridge.PSTypes        (psString)
import           Language.PureScript.Bridge.TypeInfo       (Language (Haskell))
import           Language.PureScript.Bridge.TypeParameters (A)
import           Servant                                   ((:<|>))
import           Servant.PureScript                        (HasBridge (..),
                                                            defaultSettings,
                                                            readerParams,
                                                            writeAPIModuleWithSettings)
import           System.Directory                          (copyFile,
                                                            removeFile)

import           Api                                       (HttpAppRoutes,
                                                            PlayNowRoutes)
import           Api.Types
import           Auth.Types                                (AuthToken)
import           Game.Types                                (Agent, BetState,
                                                            Card, GState, Game,
                                                            Hand, Kind, Phase,
                                                            Player, Stack,
                                                            Victory,
                                                            VictoryInfo,
                                                            VictoryType)
import           HttpApp.BotKey.Types                      (BotKey)

import qualified Game.Api.Types                            as Game
import qualified Game.Play.Api                             as Game.Play
import qualified Game.Play.Api.Types                       as Game.Play

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
  , mkSumType (Proxy :: Proxy PNActiveResp)
  , mkSumType (Proxy :: Proxy PNActive)
  , mkSumType (Proxy :: Proxy PNDeleteRq)

  , mkSumType (Proxy :: Proxy Game)
  , mkSumType (Proxy :: Proxy GState)
  , mkSumType (Proxy :: Proxy VictoryInfo)
  , mkSumType (Proxy :: Proxy VictoryType)
  , mkSumType (Proxy :: Proxy Phase)
  , mkSumType (Proxy :: Proxy Player)
  , mkSumType (Proxy :: Proxy Agent)
  , mkSumType (Proxy :: Proxy Victory)
  , mkSumType (Proxy :: Proxy Kind)
  , mkSumType (Proxy :: Proxy Hand)
  , mkSumType (Proxy :: Proxy Stack)
  , mkSumType (Proxy :: Proxy Card)
  , mkSumType (Proxy :: Proxy BetState)

  , mkSumType (Proxy :: Proxy Game.GameError)
  , mkSumType (Proxy :: Proxy Game.AuthInfo)
  , mkSumType (Proxy :: Proxy (Game.ErrorOr A))
  , mkSumType (Proxy :: Proxy Game.Play.PlayCardRq)
  , mkSumType (Proxy :: Proxy Game.Play.PlaceBetRq)
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
      pBridge = Proxy :: Proxy Bridge
      pRoutes = Proxy :: Proxy (HttpAppRoutes :<|> PlayNowRoutes)
  writeAPIModuleWithSettings settings outDir pBridge pRoutes
  writePSTypes outDir  (buildBridge bridge) types

  insertLineFile "../skull-client/src/HttpApp/BotKey/Types.purs"
                24
                "derive instance eqBotKey :: Eq BotKey"
  -- hack to create Eq instance in ps

insertLineFile :: FilePath -> Int -> Text -> IO ()
insertLineFile sourceFileName lineNumber str = do
    let tmpFilename = "tmp.txt"
    ls <- lines <$> readFile sourceFileName
    let newLs = insertAt lineNumber str ls
    writeFile tmpFilename $ unlines newLs
    removeFile sourceFileName
    copyFile tmpFilename sourceFileName
    removeFile tmpFilename
  where
    insertAt :: Int -> a -> [a] -> [a]
    insertAt n x xs = as ++ [x] ++ bs
      where
        (as, bs) = splitAt n xs
