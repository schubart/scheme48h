module Main where
import System.Environment

main :: IO ()
main = do
  putStrLn "Hi, what's your name? "
  name <- getLine
  putStrLn $ "Hello, " ++ name
