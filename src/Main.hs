{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables, ViewPatterns #-} 
module Main where
import Network.WebSockets
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

main :: IO ()
main = do
  runServer "127.0.0.1" 8080 handleConnection


parseWant :: LBS.ByteString -> Maybe Text
parseWant = T.stripPrefix "I want " . T.decodeUtf8 . LBS.toStrict

handleConnection pending = do
  connection <- acceptRequest pending
  let loop wants = do
          commandMsg <- receiveDataMessage connection
          case commandMsg of
            Text (parseWant -> Just want) -> do
              sendTextData connection
                ("Hohoho, as long as you've been good this year!" :: Text)
              loop (want : wants)
            _ -> do
              sendTextData connection ("<img src=\"http://bit.ly/1kmRC7Q\" />" :: Text)
              loop wants
  loop []
