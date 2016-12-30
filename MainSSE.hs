{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 
module Main where
import Blaze.ByteString.Builder.Char.Utf8  (fromText)
import qualified Data.ByteString.Lazy.Char8 as BL8
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
import System.IO
import System.Environment

data Message = 
      ChatMessage {
        chatName :: Text
      , chatBody :: Text
      , chatChan :: Int
      } 
    | Join { 
        joinName :: Text 
      , joinChan :: Int
      }
    | Leave { 
      leaveName :: Text 
    , leaveChan :: Int
    } deriving Show

instance FromJSON Message where
  parseJSON (Object v) = 
    (v .: "type") >>= \x ->
      case x of 
        ("chat_message" :: Text) -> 
          ChatMessage <$> v .: "name"
                      <*> v .: "body"
                      <*> v .: "chan"
        "join" -> Join <$> v .: "name" <*> v .: "chan"
        "leave" -> Leave <$> v .: "name" <*> v .: "chan"
        y -> error $ "Unrecognized Message type: " ++ show y

instance ToJSON Message where
  toJSON ChatMessage{..} = object [
      "type" .= ("chat_message" :: Text)
    , "name" .= chatName
    , "body" .= chatBody
    , "chan" .= chatChan
    ]
  toJSON (Join n ch) = object [
      "type" .= ("join" :: Text)
    , "name" .= n
    , "chan" .= ch
    ]
  toJSON (Leave n ch) = object [
      "type" .= ("leave" :: Text)
    , "name" .= n
    , "chan" .= ch
    ]

myapp :: Handle -> Chan ServerEvent -> IO Application
myapp handle chan0 = do
  sse <- sseChan chan0
  web <- scottyApp $ do 
      get "/" $ 
        file "index-sse.html"
      post "/message" $ do
        message :: Message <- jsonData
        liftIO . BL8.hPutStrLn handle . encode $ message
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
  [file] <- getArgs
  handle <- openFile file AppendMode
  hSetBuffering handle LineBuffering
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
  app <- myapp handle chan0
  putStrLn $ "port " ++ show port
  run port $ app

          
         
