{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 
module Main where
import Blaze.ByteString.Builder.Char.Utf8  (fromText)
import Data.Function (fix)
import Data.Text (Text, pack)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.UrlMap
import Network.Wai
import Network.Wai.EventSource
import Network.Wai.EventSource.EventStream
import Web.Scotty
import Control.Applicative

myapp :: IO Application
myapp = do
  let getStream :: IO ServerEvent 
      getStream = getLine >>= return . mkServerEvent
  web <- scottyApp $ do 
      get "/" $ 
        file "index.html"
      get "/style.css" $
        file "style.css"
      get "/reset.css" $
        file "reset.css"
  return $
    mapUrls $
          mount "channel" (sseChan getStream)
      <|> mountRoot web

mkServerEvent :: String -> ServerEvent
mkServerEvent s = ServerEvent Nothing Nothing [fromText . pack $  s]

sseChan :: IO ServerEvent -> Application
sseChan src = eventSourceAppIO src

main = do
  let port = 8081
  putStrLn $ "App running on port " ++ show port
  app <- myapp
  run port $ app

          
         
