{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables, BangPatterns #-} 
module Main where
import Blaze.ByteString.Builder.Char.Utf8  (fromText)
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Char8 as B8
import Data.Function (fix)
import Data.Text (Text, pack)
import qualified Data.Text.Encoding as T
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.UrlMap
import Network.Wai
import Network.HTTP.Types
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
import Data.Maybe
import           Control.Applicative      ((*>))
import           Control.Concurrent.Async (Concurrently (..))
import           Data.Conduit             (await, yield, (.|), runConduit)
import qualified Data.Conduit.Binary      as CB
import qualified Data.Conduit.List        as CL
import           Data.Conduit.Process     


data Message = 
      ChatMessage {
        chatName :: Text
      , chatBody :: Text
      , chan :: Text
      , time :: Maybe UTCTime
      } 
    | Join { 
        joinName :: Text 
      , chan :: Text
      , time :: Maybe UTCTime
      }
    | Leave { 
        leaveName :: Text 
      , chan :: Text
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
    , "chan" .= chan
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


myapp :: Chan Message -> Chan Message -> IO Application
myapp chan0 outChan = do
  let sse = sseChan chan0
  web <- scottyApp $ do 
      get "/" $ do
        liftIO $ putStrLn "get /"
        file "index.html"
      post "/message" $ do
        message :: Message <- jsonData
        now <- liftIO getCurrentTime
        let message' = message { time = Just now }
        liftIO $ writeChan outChan message'
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

sseChan :: Chan Message -> Application
sseChan chan0 req sendResponse = do
    chan' <- liftIO $ dupChan chan0
    myEventSourceApp (readChan chan') req sendResponse

myEventSourceApp :: IO Message -> Application
myEventSourceApp src req sendResponse = do
    let q = queryToQueryText $ queryString req
        chanName = fromMaybe "all" $ join $ lookup "chan" q
    sendResponse $ responseStream
        status200
        [(hContentType, "text/event-stream")]
        $ \sendChunk flush -> fix $ \loop -> do
            m :: Message <- src
            let se = filterChan chanName m
                     >>= Just . mkServerEvent
                     >>= eventToBuilder
            case se of
                Nothing -> loop
                Just b  -> sendChunk b >> flush >> loop

mkServerEvent :: Message -> ServerEvent
mkServerEvent m = 
    let json = T.decodeUtf8 . BL8.toStrict $ encode m
    in ServerEvent Nothing Nothing [fromText json]

filterChan :: Text -> Message -> Maybe Message
filterChan "all" x = Just x
filterChan chan' x = 
    if (chan x) == chan' || (chan x) == "all"
    then Just x
    else Nothing

main = do
  [file] <- getArgs
  handle <- openFile file AppendMode
  hSetBuffering handle LineBuffering
  hSetBuffering stderr LineBuffering
  hSetBuffering stdout LineBuffering
  let port = 3001
  putStrLn $ "App running on port " ++ show port
  chan0 :: Chan Message <- newChan 

  outChan :: Chan Message <- newChan
  putStrLn "hello1"
  (ClosedStream, fromTail, ClosedStream, cph) <- streamingProcess (proc "tail" ["-100", "-f", "log"])
  putStrLn "hello"
  let input = runConduit $ fromTail 
                        .| CB.lines
                        .| CL.mapM_
        (\bs -> 
          let v = decode $ BL8.fromStrict bs
          in case v of
              Just v' -> do
                writeChan chan0 v' 
              _ -> do
                hPutStrLn stderr $ "error reading input " ++ show v
                return ()
        )

  ec <- runConcurrently 
        $ Concurrently input
        -- *> Concurrently (waitForStreamingProcess cph)
        *> Concurrently (
              -- buffered output
              -- This throttles output to ensure outgoing JSON messages don't overlap 
              fix $ \loop -> do
                m :: Message <- readChan outChan
                BL8.hPutStrLn handle . encode $ m
                loop)
        *> Concurrently (do
            putStrLn "Running server"
            app <- myapp chan0 outChan
            putStrLn $ "port " ++ show port
            run port $ app
        )

  putStrLn $ "Process exit code: " ++ show ec

