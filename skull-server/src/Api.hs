{-# LANGUAGE TypeOperators #-}

module Api
  ( Routes
  ) where

import           Servant     ((:<|>), Raw)

import qualified HttpApp.Api as HttpApp

type Routes  =
       HttpApp.Routes
  :<|> Raw
