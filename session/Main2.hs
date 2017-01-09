{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 

module Main where
import Web.ClientSession
import qualified Data.ByteString.Char8 as B8

main = do
  key <- getDefaultKey
  s <- B8.getContents 
  let r = decrypt key s
  print r



