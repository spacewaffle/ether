{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 
module Main where
import Blaze.ByteString.Builder.Char.Utf8  (fromText)
import Data.Function (fix)
import Data.Text (Text, pack)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.UrlMap
import Network.Wai
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.IO.Class (liftIO)
import Network.Wai.EventSource
import Network.Wai.EventSource.EventStream
import Web.Scotty
import Control.Applicative
import Data.Aeson

data ChatMessage = 
      ChatMessage {
        chatName :: Text
      , chatBody :: Text
      } deriving Show

data Join = Join { joinName :: Text }

data Leave = Leave { leaveName :: Text }

instance FromJSON ChatMessage where
  parseJSON (Object v) = ChatMessage <$> v .: "name"
                                     <*> v .: "body"

instance ToJSON ChatMessage where
  toJSON ChatMessage{..} = object [
      "name" .= chatName
    , "body" .= chatBody
    ]


myapp :: Chan ServerEvent -> IO Application
myapp chan0 = do
  sse <- sseChan chan0
  web <- scottyApp $ do 
      get "/" $ 
        file "index.html"
      post "/message" $ do
        undefined
        -- append to STDOUT or unix style file handle
        
      get "/style.css" $
        file "style.css"
      get "/reset.css" $
        file "reset.css"

  return $
    mapUrls $
          mount "sse" sse
      <|> mountRoot web

mkServerEvent :: String -> ServerEvent
mkServerEvent s = ServerEvent Nothing Nothing [fromText . pack $  s]

sseChan :: Chan ServerEvent -> IO Application
sseChan chan0 = do
    chan <- dupChan chan0
    return $ eventSourceAppChan chan

main = do
  let port = 8081
  putStrLn $ "App running on port " ++ show port
  chan0 <- newChan 
  forkIO $ do 
      fix $ \loop -> do
        line <- getLine 
        putStrLn $ "Data: " ++ line
        writeChan chan0 $ mkServerEvent line 
        loop
  putStrLn "Running server"
  app <- myapp chan0
  putStrLn $ "port " ++ show port
  run port $ app

          
         
