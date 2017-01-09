{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 

module Main where
import Web.ClientSession
import qualified Data.ByteString.Char8 as B8

main = do
  key <- getDefaultKey
  iv <- randomIV
  let r = encrypt key iv "hello" 
  B8.putStr r


