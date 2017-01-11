{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 
module User where
import Database.PostgreSQL.Simple
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Int (Int64)
import Data.Maybe 
import Control.Applicative
import Lucid

import Text.Digestive.Lucid.Html5
import Text.Digestive

import qualified Web.Scotty as W
import Web.Scotty (ActionM)
import Text.Digestive.Scotty (runForm)

-- https://hackage.haskell.org/package/digestive-functors-lucid-0.0.0.4/docs/Text-Digestive-Lucid-Html5.html


data User = 
    User 
      Int64 -- id
      Text -- username
      Text -- email 
  deriving Show

getUserById :: Connection -> Int64 -> IO (Maybe User)
getUserById c uid = 
    query c "select id, username, email from users where id = ?" (Only uid)
    >>= \xs -> 
      case xs of { (i, u, e):_ -> return (Just $ User i u e) ; _ -> return Nothing }

------------------------------------------------------------------------
data UserCreate = 
    UserCreate 
        Text  -- username 
        Text  -- email
        Text  -- validated password
    deriving Show

-- TODO encrypt password

createUser :: Connection -> UserCreate -> IO User
createUser c (UserCreate u e p) = do
    x <- query c "insert into users (username, email, encrypted_password) \
         \values (?, ?, ?) returning id" (u, e, p)
    -- may get username uniqueness exception
    case x of
      (Only uid):_ -> fromJust <$> getUserById c uid
      _ -> error ("Failed to create user: " ++ show u)

signupForm :: Monad m => Form Text m UserCreate
signupForm = UserCreate 
    <$> "username" .: text Nothing
    <*> "email" .: text Nothing
    <*> "password" .: text Nothing

signupHtml :: View Text -> Html ()
signupHtml view = do
  form_ [acceptCharset_ "UTF-8", action_ "/signup", method_ "POST"] $ do
    input_ [ name_ $ absoluteRef "username" view
           , placeholder_ "Username"
           , size_ "20"
           , type_ "text"
           , value_ $ fieldInputText "username" view
           ]
    inputText "email" view
    inputPassword "password" view
    inputSubmit "Save"

signupAction :: ActionM ()
signupAction = do
  r <- runForm "signup" signupForm
  case r of
    (view, Nothing) -> W.html $ renderText (signupHtml view)
    (view, Just x) -> W.text $ TL.pack ("Create User: " <> show x)

------------------------------------------------------------------------

