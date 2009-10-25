module IOPrimitives (ioPrimitives) where

import Types
import IO (stdout, hPrint)
import Control.Monad.Error (liftIO)

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("write", writeProc)]

writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port handle] = do liftIO $ hPrint handle obj
                                  return $ LBool True
