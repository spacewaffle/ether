{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables, FlexibleInstances #-} 
module Main where
import           Control.Applicative      ((*>))
import           Data.Conduit             
import qualified Data.Conduit.Binary      as CB
import qualified Data.Conduit.List        as CL
import           Data.Conduit.Process     (ClosedStream (..), streamingProcess,
                                           proc, waitForStreamingProcess)
import           System.IO                (stdin)
import qualified Data.ByteString.Char8 as B8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid


main = do
  conduitQuery  "select * from users" >>= T.putStrLn

conduitQuery :: String -> IO Text
conduitQuery  query = do
    (ClosedStream, fromProcess, ClosedStream, cph) <-
        streamingProcess 
          (proc "mysql"  $
              [ 
                "-uroot"
              , "redesign"
              , "-t"
              , "-e"
              , query
              ])
    output <- runConduit $ fromProcess =$= CL.map T.decodeUtf8 =$ CL.consume
    return $ T.unlines output 
        


