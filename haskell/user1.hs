{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 
module Main where
import Database.PostgreSQL.Simple
import Data.Monoid
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Int (Int64)
import Data.Maybe 
import Control.Applicative
import System.Environment
data User = 
    User {
      userId :: Int64 -- id
    , username :: Text -- username
    , userEmail :: Text -- email 
    }
  deriving Show


getUserById :: Connection -> Int64 -> IO (Maybe User)
getUserById c uid = 
    query c "select user_id, username, email from users where user_id = ?" (Only uid)
    >>= \xs -> 
      case xs of { (i, u, e):_ -> return (Just $ User i u e) ; _ -> return Nothing }

main = do
  [uid] <- getArgs
  c <- connectPostgreSQL "dbname=ether host=localhost"
  r <- getUserById c (read uid)
  print r
