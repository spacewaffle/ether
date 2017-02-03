{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables, BangPatterns #-} 
module Main where
import User
import qualified Blaze.ByteString.Builder as Builder 
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Char8 as B8
import Data.Function (fix)
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.UrlMap
import Network.Wai.Middleware.Autohead
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.AddHeaders

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)

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
import qualified Web.Scotty as W
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
import Cookie

data Message = 
      ChatMessage {
        chatName :: Maybe Text
      , chatBody :: Text
      , chan :: Text
      , time :: Maybe UTCTime
      } 
    | GetUsername { 
        chatName :: Maybe Text 
      , chan :: Text
      , time :: Maybe UTCTime
      }
    | Leave { 
        chatName :: Maybe Text 
      , chan :: Text
      , time :: Maybe UTCTime
      } deriving Show
    -- a Ping type to signal still part of room?

instance FromJSON Message where
  parseJSON (Object v) = 
    (v .: "type") >>= \x ->
      case x of 
        ("chat_message" :: Text) -> 
          ChatMessage <$> v .:? "name"
                      <*> v .: "body"
                      <*> v .: "chan"
                      <*> v .:? "time"
        "askUsername" -> GetUsername 
              <$> v .:? "name" <*> v .: "chan" <*> v .:? "time"
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
  toJSON (GetUsername n ch t) = object [
      "type" .= ("askUsername" :: Text)
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

instance ToJSON User where
  toJSON User{..} = object [ "username" .= username ]

data Redirect = Redirect Text -- location

instance ToJSON Redirect where
  toJSON (Redirect x) = object [ "type" .= (String "redirect") , "url" .= x ]

redirectTo :: Text -> ActionM ()
redirectTo loc = W.json (Redirect loc)

withUser :: (User -> ActionM ()) -> ActionM ()
withUser f = do
      muid <- getSessionUserId 
      case muid of
        Nothing -> redirectTo "/login"
        Just uid -> do
          mUser <- liftIO $ getUserById' uid
          case mUser of
            (Just u) -> f u
            Nothing -> redirectTo "/login"

myapp :: Chan Message -> Chan Message -> IO Application
myapp chan0 outChan = do
  let sse = sseChan 
  web <- scottyApp $ do 
      get "/" $ file "index.html"
      get "/login" $ loginAction
      post "/login" $ loginAction
      get "/logout" $ do
        setHeader "Set-Cookie" ""
        redirect "/login"
      get "/signup" $ signupAction
      post "/signup" $ signupAction

      get "/user_info" $ 
          withUser $ \user -> W.json user
        
      post "/message" $ do
        withUser $ \user -> do
          now <- liftIO getCurrentTime
          message :: Message <- jsonData
          let message' = message { time = Just now , chatName = (Just (username user)) }
          liftIO $ writeChan outChan message'

      get "/chan/:id" $ do
        -- this should present a backlog of n messages from the file
        undefined
      get "/chats.js" $ do
        setHeader "Content-Type" "application/javascript"
        file "chats.js"
      get "/style.css" $ do
        setHeader "Content-Type" "text/css"
        file "style.css"
      get "/reset.css" $ do
        setHeader "Content-Type" "text/css"
        file "reset.css"
  return $
    autohead $ 
    -- logStdout $
    -- addHeaders [("test", "header")] $
    mapUrls $
          mount "sse" sse
      <|> mountRoot web

sseChan :: Application
sseChan req sendResponse = do
    myEventSourceApp req sendResponse
  where
    myEventSourceApp :: Application
    myEventSourceApp req sendResponse = do
        let q = queryToQueryText $ queryString req
            chanName = fromMaybe "all" $ join $ lookup "chan" q
            chanFile = T.unpack ("chan/" <> chanName)
        putStrLn $ "tail -f " ++ chanFile
        (ClosedStream, fromTail, ClosedStream, cph) <- streamingProcess (shell $ "tail -f " ++ chanFile)
        writeFile chanFile ""
        sendResponse $ responseStream
            status200
            [(hContentType, "text/event-stream")]
            $ \sendChunk flush -> do
                runConduit $ fromTail 
                    .| CB.lines
                    .| CL.mapM_
                    (\bs -> do
                      let bs' = Builder.fromByteString bs
                      case eventToBuilder (ServerEvent Nothing Nothing [bs']) of
                        Nothing -> return ()
                        Just b -> sendChunk b >> flush 
                    )



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
  (ClosedStream, fromTail, ClosedStream, cph) <- streamingProcess (shell "tail -f log")
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

