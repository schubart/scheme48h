module Eval (eval, bindVars) where

import Types
import Control.Monad.Error (throwError, liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import Monad (liftM)

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _   val@(LString _) = return val
eval _   val@(LNumber _) = return val
eval _   val@(LBool _) = return val
eval env (LAtom id) = getVar env id
eval _   (LList [LAtom "quote", val])     = return val
eval env (LList [LAtom "if", cond, t, e]) = do result <- eval env cond
                                               case result of
                                                 LBool True  -> eval env t
                                                 LBool False -> eval env e
                                                 x -> throwError $ 
                                                      TypeMismatch "boolean" x
eval env (LList [LAtom "set!", LAtom var, expr]) = eval env expr >>= setVar env var
eval env (LList [LAtom "define", LAtom var, expr]) = eval env expr >>= defineVar env var
eval env (LList (LAtom "define" : LList (LAtom name : params) : body)) = 
    makeFunc env params body >>= defineVar env name
eval env (LList (LAtom "lambda" : LList params : body)) = makeFunc env params body
eval env (LList (function : args)) = do func <- eval env function
                                        argVals <- mapM (eval env) args
                                        apply func argVals
eval _   badForm  = throwError $ UnrecognizedSpecialForm badForm

-- TODO what if params are not strings or symbols?
makeFunc env params body = return $ Func (map show params) Nothing body env

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args         = liftThrows $ func args
apply (IOFunc func) args                = func args
apply (Func params _ body closure) args = 
    if length args /= length params
       then throwError $ NumArgs (length params) args
       else (liftIO $ bindVars closure $ zip params args) >>= evalBody
    -- TODO varargs
    where
      -- Eval each statement, keep result of last one.
      -- TODO: crashes if body empty.
      evalBody env = liftM last $ mapM (eval env) body

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

isBound :: Env -> String -> IO Bool
isBound envRef var = do env <- readIORef envRef
                        return $ maybe False
                                       (const True)
                                       (lookup var env)

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar var)
                             (liftIO . readIORef)
                             (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var val = do env <- liftIO $ readIORef envRef
                           maybe (throwError $ UnboundVar var)
                                 (liftIO . (flip writeIORef val))
                                 (lookup var env)
                           return val

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var val = do
  defined <- liftIO $ isBound envRef var
  if defined
     then do setVar envRef var val
             return val
     else liftIO $ do
       valRef <- newIORef val
       env <- readIORef envRef
       writeIORef envRef ((var, valRef) : env)
       return val

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)
