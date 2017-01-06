#!/usr/bin/env stack
-- stack --resolver lts-6.19 --install-ghc runghc --package conduit-extra
{-# LANGUAGE OverloadedStrings #-}
import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (Concurrently (..))
import qualified Data.ByteString.Char8    as S8
import           Data.Conduit             (yield, runConduit, (.|))
import           Data.Conduit.Binary      (sourceHandle)
import qualified Data.Conduit.List        as CL
import           Data.Conduit.Process     (streamingProcess, proc, terminateProcess, streamingProcessHandleRaw, waitForStreamingProcess)
import           System.Posix.IO          (closeFd, fdToHandle, createPipe)

main :: IO ()
main = do
    (reader, writer) <- createPipe

    writeFile "script.sh" $ unlines
        [ "trap 'echo Got TERM; exit 42' TERM"
        , "echo Sending to standard output"
        , "echo Sending to standard error >&2"
        , "echo Sending to our pipe " ++ show writer ++ " >&" ++ show writer
        , "cat # Print everything from stdin to stdout"
        , "while true; do sleep 1; done"
        ]

    readerH <- fdToHandle reader

    ((input, close), out, err, cph)
        <- streamingProcess (proc "bash" ["script.sh"])

    closeFd writer

    let go src name =
            Concurrently $ runConduit $ src
            .| CL.mapM_ (\bs -> S8.putStr $ S8.pack $ name ++ ": " ++ show bs ++ "\n")
        feed = Concurrently $ do
            threadDelay 1000000
            runConduit
                $ yield "Feeding standard input, then terminating"
               .| input
            terminateProcess $ streamingProcessHandleRaw cph
            close

    runConcurrently
         $ go out "stdout"
        *> go err "stderr"
        *> go (sourceHandle readerH) "pipe"
        *> feed
        *> Concurrently (waitForStreamingProcess cph >>= print)
