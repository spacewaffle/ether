{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 

module Main where
import Web.Scotty
import Network.Wai.Handler.Warp
import qualified Data.Text.Lazy as TL
import Control.Monad.IO.Class
import Data.Aeson

main = do
  app <- scottyApp $ do
    post "/" $ do
      ps <- params
      -- liftIO $ print ps
      b <- body
      -- liftIO $ print b
      let r = encode ps
      
      raw r

  run 3011 $ app

