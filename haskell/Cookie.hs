{-# LANGUAGE OverloadedStrings #-} 
module Cookie where
import Web.Cookie
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Builder as BL
import qualified Data.Map as M
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Control.Monad (liftM)
import Web.Scotty
import Data.Int

getCookie :: ByteString -> ActionM (Maybe B8.ByteString)
getCookie cookieName = liftM (M.lookup cookieName) getCookies

getCookies :: ActionM (M.Map ByteString ByteString)
getCookies = liftM (M.fromList . maybe [] parse) $ header "Cookie"
    where parse :: TL.Text -> [(ByteString, ByteString)]
          parse = parseCookies . BL.toStrict . TL.encodeUtf8

setCookie :: SetCookie -> ActionM ()
setCookie c = addHeader "Set-Cookie" (TL.decodeUtf8 . BL.toLazyByteString $ renderSetCookie c)

setSessionId :: Int64 -> ActionM ()
setSessionId n = 
    setCookie $ def { setCookieName = "_sessid"
                    , setCookieValue = (B8.pack . show $ n)  -- not safe, change later
                    , setCookieHttpOnly = True }

getSessionUserId :: ActionM (Maybe Int64)
getSessionUserId = do
    b <- getCookie "_sessid" 
    return . fmap (read . B8.unpack) $ b



--                 let c = def { setCookieName = _sessionCookieName
--                             , setCookieValue = sUUID'
--                             , setCookieHttpOnly = True }
--                 setCookie c
-- 
--           let c = def { setCookieName = _sessionCookieName
--                       , setCookieValue = "" 
--                       , setCookieExpires = Just $ posixSecondsToUTCTime 0 }
--           setCookie c
-- 
