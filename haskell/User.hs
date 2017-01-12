{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 
module User where
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
import Lucid
import Web.Cookie

import Text.Digestive.Lucid.Html5
import Text.Digestive

import qualified Web.Scotty as W
import Web.Scotty (ActionM)
import Text.Digestive.Scotty (runForm)

import Network.Wai (Application)
import Cookie

-- https://hackage.haskell.org/package/digestive-functors-lucid-0.0.0.4/docs/Text-Digestive-Lucid-Html5.html

data User = 
    User 
      Int64 -- id
      Text -- username
      Text -- email 
  deriving Show

getUserById :: Connection -> Int64 -> IO (Maybe User)
getUserById c uid = 
    query c "select user_id, username, email from users where user_id = ?" (Only uid)
    >>= \xs -> 
      case xs of { (i, u, e):_ -> return (Just $ User i u e) ; _ -> return Nothing }


withConnection :: (Connection -> IO a) -> IO a
withConnection f = do
  c <- connectPostgreSQL "dbname=ether host=localhost" 
  r <- f c
  close c 
  return r

login :: Connection -> Text -> Text -> IO (Maybe Int64)
login c u p = 
    query c "select user_id from users where username = ?  and encrypted_password = ?" (u, p)
    >>= \xs -> return $ listToMaybe [ i | (Only i) <- xs ]

login' u p = withConnection (\c -> login c u p)

------------------------------------------------------------------------
data UserCreate = 
    UserCreate 
        Text  -- username 
        Text  -- email
        Text  -- validated password
    deriving Show

-- TODO encrypt password
-- may get username uniqueness exception

createUser :: Connection -> UserCreate -> IO User
createUser c (UserCreate u e p) = do
    x <- query c "insert into users (username, email, encrypted_password) \
         \values (?, ?, ?) returning user_id" (u, e, p)
    case x of
      (Only uid):_ -> fromJust <$> getUserById c uid
      _ -> error ("Failed to create user: " ++ show u)

createUser' :: UserCreate -> IO User
createUser' x = withConnection (flip createUser x)


loginAction :: ActionM ()
loginAction = do
  r <- runForm "login" loginForm
  case r of 
    (view, Nothing) -> W.html . renderText $ loginHtml view Nothing
    (view, Just (Login u p)) -> do
      s <- liftIO $ login' u p
      case s of
        Just uid -> do
            setSessionId uid
            W.redirect "/"
        Nothing -> W.html . renderText $ loginHtml view (Just "Login failed")

-- can thread layout in here
signupAction :: ActionM ()
signupAction = do
  r <- runForm "signup" signupForm
  case r of
    (view, Nothing) -> W.html $ renderText (signupHtml view)
    (_, Just x) -> do
        u <- liftIO (createUser' x)
        W.redirect "/login"

data Login = Login Text Text

loginForm = Login
    <$> "username" .: usernameValid
    <*> "password" .: passwordValid

loginHtml :: View (Html ()) -> Maybe Text -> Html ()
loginHtml view err = do
  maybe mempty (p_ [class_ "error"] . toHtml) err 
  form_ [acceptCharset_ "UTF-8", action_ "/login", method_ "POST"] $ do
    input_ [ name_ $ absoluteRef "username" view
           , placeholder_ "Username"
           , size_ "20"
           , type_ "text"
           , value_ $ fieldInputText "username" view
           ]
    error_list "username" view
    br_ []
    inputPassword "password" view
    error_list "password" view
    br_ []
    inputSubmit "Log in"
  p_ $ a_ [href_ "/signup"] "Sign up"

signupForm :: Monad m => Form (Html ()) m UserCreate
signupForm = UserCreate 
    <$> "username" .: usernameValid
    <*> "email" .: emailValid
    <*> "password" .: passwordValid

usernameValid, emailValid, passwordValid :: Monad m => Form (Html ()) m Text 

usernameValid = check "Username can't be blank" checkNotBlank (text Nothing)
emailValid = check "Email can't be blank" checkNotBlank (text Nothing)
passwordValid = check "Password can't be blank" checkNotBlank (text Nothing)

checkNotBlank :: Text -> Bool
checkNotBlank = not . T.null . T.strip

signupHtml :: View (Html ()) -> Html ()
signupHtml view = do
  form_ [acceptCharset_ "UTF-8", action_ "/signup", method_ "POST"] $ do
    input_ [ name_ $ absoluteRef "username" view
           , placeholder_ "Username"
           , size_ "20"
           , type_ "text"
           , value_ $ fieldInputText "username" view
           ]
    error_list "username" view
    br_ []
    inputText "email" view
    error_list "email" view
    br_ []
    inputPassword "password" view
    error_list "password" view
    br_ []
    inputSubmit "Save"

error_list :: Monad m => Text -> View (HtmlT m ()) -> HtmlT m ()
error_list ref view = case errors ref view of
    []   -> mempty
    errs -> ul_ [class_ "error-list"] $ forM_ errs $ \e ->
              li_ [class_ "error"] e

------------------------------------------------------------------------

