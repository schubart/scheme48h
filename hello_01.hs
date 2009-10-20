module Main where
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ unwords $ "Hello," : take 2 args
 