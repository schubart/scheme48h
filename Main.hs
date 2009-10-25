module Main where
import Types
import Parser
import Eval
import Primitives
import IOPrimitives
import Data.IORef (newIORef)
import IO (hFlush, stdout)
import Control.Monad.Error (runErrorT)

main :: IO ()
main = newIORef [] 
       >>= (flip bindVars (map (makeFunc PrimitiveFunc) primitives ++
                           map (makeFunc IOFunc)        ioPrimitives))
       >>= repl
    where makeFunc ctor (name, func) = (name, ctor func)

repl :: Env -> IO ()
repl env = do putStr "Lisp > "
              hFlush stdout
              line <- getLine
              case line of
                "quit" -> return ()
                otherwise -> do result <- runErrorT $ (readExpr line) >>= eval env
                                case result of
                                  Left err  -> print err
                                  Right val -> print val
                                repl env
