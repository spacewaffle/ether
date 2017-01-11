{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 
module User where
import Database.PostgreSQL.Simple
import Data.Text (Text)
import Data.Int (Int64)
import Data.Maybe 
import Control.Applicative

data User = 
    User 
      Int64 -- id
      Text -- username
      Text -- email 


getUserById :: Connection -> Int64 -> IO (Maybe User)
getUserById c uid = 
    query c "select id, username, email from users where id = ?" (Only uid)
    >>= \xs -> 
          case xs of 
              (i, u, e):_ -> return $ Just $ User i u e
              _ -> return Nothing


------------------------------------------------------------------------
data UserCreate = 
    UserCreate 
        Text  -- username 
        Text  -- email

createUser :: Connection -> UserCreate -> IO User
createUser c (UserCreate u e) = do
    x <- query c "insert into users (username, email) values (?, ?) returning id" (u, e)
    -- may get username uniqueness exception
    case x of
      (Only uid):_ -> fromJust <$> getUserById c uid
      _ -> error ("Failed to create user: " ++ show u)

------------------------------------------------------------------------

