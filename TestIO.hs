module Main where
import           Data.Function (fix)

main = do
  putStrLn "hello"
  fix $ \loop -> do
          se <- getLine
          putStrLn se >> loop
          
         
