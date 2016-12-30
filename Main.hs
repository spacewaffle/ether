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
import Control.Monad (join)
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.IO.Class (liftIO)
import Network.Wai.EventSource
import Network.Wai.EventSource.EventStream
import Network.HTTP.Types (status200, hContentType)
import Web.Scotty
import Control.Applicative
import Data.Aeson
import System.IO
import System.Environment
import Data.Time.Clock
import Data.Monoid

data Message = 
      ChatMessage {
        chatName :: Text
      , chatBody :: Text
      , chatChan :: Text
      , time :: Maybe UTCTime
      } 
    | Join { 
        joinName :: Text 
      , joinChan :: Text
      , time :: Maybe UTCTime
      }
    | Leave { 
        leaveName :: Text 
      , leaveChan :: Text
      , time :: Maybe UTCTime
      } deriving Show
    -- a Ping type to signal still part of room?

instance FromJSON Message where
  parseJSON (Object v) = 
    (v .: "type") >>= \x ->
      case x of 
        ("chat_message" :: Text) -> 
          ChatMessage <$> v .: "name"
                      <*> v .: "body"
                      <*> v .: "chan"
                      <*> v .:? "time"
        "join" -> Join <$> v .: "name" <*> v .: "chan" <*> v .:? "time"
        "leave" -> Leave <$> v .: "name" <*> v .: "chan" <*> v .:? "time"
        y -> error $ "Unrecognized Message type: " ++ show y

instance ToJSON Message where
  toJSON ChatMessage{..} = object [
      "type" .= ("chat_message" :: Text)
    , "name" .= chatName
    , "body" .= chatBody
    , "chan" .= chatChan
    , "time" .= time
    ]
  toJSON (Join n ch t) = object [
      "type" .= ("join" :: Text)
    , "name" .= n
    , "chan" .= ch
    , "time" .= t
    ]
  toJSON (Leave n ch t) = object [
      "type" .= ("leave" :: Text)
    , "name" .= n
    , "chan" .= ch
    , "time" .= t
    ]

myapp :: Handle -> Chan ServerEvent -> IO Application
myapp handle chan0 = do
  let sse = sseChan chan0
  web <- scottyApp $ do 
      get "/" $ 
        file "index.html"
      post "/message" $ do
        message :: Message <- jsonData
        now <- liftIO getCurrentTime
        let message' = message { time = Just now }
        liftIO . BL8.hPutStrLn handle . encode $ message'
      get "/chan/:id" $ do
        -- this should present a backlog of n messages from the file
        undefined
      get "/style.css" $ do
        setHeader "Content-Type" "text/css"
        file "style.css"
      get "/reset.css" $ do
        setHeader "Content-Type" "text/css"
        file "reset.css"

  return $
    mapUrls $
          mount "sse" sse
      <|> mountRoot web

mkServerEvent :: String -> ServerEvent
mkServerEvent s = ServerEvent Nothing Nothing [fromText . pack $ s]

sseChan :: Chan ServerEvent -> Application
sseChan chan0 req sendResponse = do
    chan' <- liftIO $ dupChan chan0
    myEventSourceApp (readChan chan') req sendResponse


myEventSourceApp :: IO ServerEvent -> Application
myEventSourceApp src req sendResponse = do
    let q = queryString req
        chan = join $ lookup "chan" q
    -- TODO capture channel number
    liftIO $ hPutStrLn stderr $ "chan: " ++ show chan

    sendResponse $ responseStream
        status200
        [(hContentType, "text/event-stream")]
        $ \sendChunk flush -> fix $ \loop -> do
            se <- src


            case eventToBuilder se of
                Nothing -> return ()
                Just b  -> sendChunk b >> flush >> loop



main = do
  [file] <- getArgs
  handle <- openFile file AppendMode
  hSetBuffering handle LineBuffering
  hSetBuffering stderr LineBuffering
  hSetBuffering stdout LineBuffering
  let port = 8081
  putStrLn $ "App running on port " ++ show port
  chan0 <- newChan 
  forkIO $ do 
      fix $ \loop -> do
        line <- getLine 
        putStrLn line
        writeChan chan0 $ mkServerEvent line 
        loop
  putStrLn "Running server"
  app <- myapp handle chan0
  putStrLn $ "port " ++ show port
  run port $ app

          
         
