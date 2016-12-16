{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables, ViewPatterns #-} 
module Main where
import Network.WebSockets
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Data.Aeson
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets
import Network.Wai.UrlMap
import Network.Wai
import Network.HTTP.Types
import Web.Scotty
import Control.Applicative

type Client = (Text, Connection)
type ServerState = [Client]

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
    T.putStrLn message
    forM_ clients $ \(_, conn) -> sendTextData conn message

addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

parseWant :: LBS.ByteString -> Maybe Text
parseWant = T.stripPrefix "I want " . T.decodeUtf8 . LBS.toStrict


-- scottyApp :: ScottyM () -> IO Application
-- run :: Port -> Application -> IO ()

myapp :: MVar ServerState -> IO Application
myapp state = do
  web <- scottyApp $ do 
      get "/" $ 
        text "hello"
  return $
    mapUrls $
          mount "ws" (wsApp state)
      <|> mountRoot web


wsApp :: MVar ServerState -> Application
wsApp state = websocketsOr defaultConnectionOptions 
              (handleConnection state) backupApp
  where
    backupApp :: Application
    backupApp _ respond = respond $ responseLBS status400 [] "Not a WebSocket request"

main :: IO ()
main = do
  state <- newMVar []
  runServer "127.0.0.1" 8081 (handleConnection state)

handleConnection :: MVar ServerState -> ServerApp
handleConnection state pending = do
  connection <- acceptRequest pending
  forkPingThread connection 30
  name <- receiveData connection
  let client = (name , connection)
  let disconnect = do
          s <- modifyMVar state $ \s -> 
              let s' :: ServerState
                  s' = removeClient client s 
              in return (s', s')
          broadcast (fst client `mappend` " disconnected") s
  flip finally disconnect $ do
    modifyMVar_ state $ \s -> do
      let s' = addClient client s
      sendTextData connection $
        "Welcome! Users: " `mappend`
        T.intercalate ", " (map fst s)
      broadcast (fst client `mappend` " joined") s'
      return s'
    talk connection state client


talk :: Connection -> MVar ServerState -> Client -> IO ()
talk conn state (user, _) = forever $ do
  msg <- receiveData conn
  readMVar state >>= broadcast
    (user `mappend` ": " `mappend` msg)

{-
http://hackage.haskell.org/package/websockets-0.10.0.0/docs/Network-WebSockets.html

sendTextData :: WebSocketsData a => Connection -> a -> IO ()
sendTextDatas :: WebSocketsData a => Connection -> [a] -> IO ()


-}
