{-# LANGUAGE OverloadedStrings #-} 
module Main where
import Web.Cookie
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment

main = do
  [name, value] <- getArgs
  let cookie = def { setCookieName = B8.pack name
                    , setCookieValue = B8.pack value
                    , setCookieHttpOnly = True }
  let r = renderSetCookie cookie 
  BL8.putStrLn $ Builder.toLazyByteString r
