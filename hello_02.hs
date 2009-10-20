module Main where
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  putStr "Sum of first two arguments: "
  print $ sum $ map read $ take 2 args
 