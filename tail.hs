#!/usr/bin/env stack
-- stack --resolver lts-6.19 --install-ghc runghc --package conduit-extra
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative      ((*>))
import           Control.Concurrent.Async (Concurrently (..))
import           Data.Conduit             (await, yield, (.|), runConduit)
import qualified Data.Conduit.Binary      as CB
import qualified Data.Conduit.List        as CL
import           Data.Conduit.Process     
import           System.IO                (stdin)

main :: IO ()
main = do
    putStrLn "Doing tail -f log2"

    (ClosedStream, fromProcess, ClosedStream, cph) <-
        streamingProcess (proc "tail" ["-f", "log2"])


    let output = runConduit $ fromProcess .| CL.mapM_
            (\bs -> putStrLn $ "from process: " ++ show bs)

    ec <- runConcurrently $
        Concurrently output *>
        Concurrently (waitForStreamingProcess cph)

    putStrLn $ "Process exit code: " ++ show ec
