module Primitives (primitives) where

import Types
import Control.Monad.Error (throwError)

primitives ::[(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+",         numericBinop (+))
             ,("-",         numericBinop (-))
             ,("*",         numericBinop (*))
             ,("/",         numericBinop div)
             ,("mod",       numericBinop mod)
             ,("quotient",  numericBinop quot)
             ,("remainder", numericBinop rem)
             -- Exercise 3.1
             ,("boolean?", unaryOp booleanp)
             ,("symbol?",  unaryOp symbolp)
             ,("string?",  unaryOp stringp)
             ,("number?",  unaryOp numberp)
             -- Exercise 3.3
             ,("string->symbol", unaryOp string2symbol)
             ,("symbol->string", unaryOp symbol2string)
             ,("=",  numBoolBinOp (==))
             ,("<",  numBoolBinOp (<))
             ,(">",  numBoolBinOp (>))
             ,("/=", numBoolBinOp (/=))
             ,("<=", numBoolBinOp (<=))
             ,(">=", numBoolBinOp (>=))
             ,("&&", boolBoolBinOp (&&))
             ,("||", boolBoolBinOp (||))
             ,("string=?",  strBoolBinOp (==))
             ,("string<?",  strBoolBinOp (<))
             ,("string>?",  strBoolBinOp (>))
             -- TODO: Why no "string/=?"?
             ,("string<=?", strBoolBinOp (<=))
             ,("string>=?", strBoolBinOp (>=))

             ,("car",    car)
             ,("cdr",    cdr)
             ,("cons",   cons)
             ,("eq?",    eqvp)
             ,("eqv?",   eqvp)
             ]

-- TODO: "equal?"
-- TODO: Exercises 4.*

-- Helpers for creating primitves.
numericBinop :: (Integer -> Integer -> Integer) 
             -> [LispVal] -> ThrowsError LispVal
numericBinop op args = if length args < 2
                       then throwError $ NumArgs 2 args
                       else mapM unpackNum args >>= return . LNumber . foldl1 op

unaryOp :: (LispVal -> ThrowsError LispVal)
        -> [LispVal] -> ThrowsError LispVal
unaryOp op [arg] = op arg
unaryOp _ args = throwError $ NumArgs 1 args

boolBinOp :: (LispVal -> ThrowsError a)
          -> (a -> a -> Bool)
          -> [LispVal] -> ThrowsError LispVal
boolBinOp unpacker op args = if length args < 2
                             then throwError $ NumArgs 2 args
                             else do unpacked <- mapM unpacker args
                                     return $ LBool $ pairwiseTrue op unpacked

pairwiseTrue :: (a -> a -> Bool) -> [a] -> Bool
pairwiseTrue op args = and $ zipWith op args (tail args)

numBoolBinOp  = boolBinOp unpackNum
boolBoolBinOp = boolBinOp unpackBool
strBoolBinOp  = boolBinOp unpackStr

-- Helpers for unpacking.
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (LNumber n) = return n
unpackNum x           = throwError $ TypeMismatch "number" x

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (LBool b) = return b
unpackBool x         = throwError $ TypeMismatch "boolean" x

unpackStr :: LispVal -> ThrowsError String
unpackStr (LString s) = return s
unpackStr x           = throwError $ TypeMismatch "string" x

-- Implementations.
booleanp (LBool _) = return $ LBool True
booleanp _         = return $ LBool False

symbolp (LAtom _) = return $ LBool True
symbolp _         = return $ LBool False

stringp (LString _) = return $ LBool True
stringp _           = return $ LBool False

numberp (LNumber _) = return $ LBool True
numberp _           = return $ LBool False

string2symbol (LString val) = return $ LAtom val
string2symbol x             = throwError $ TypeMismatch "string" x

symbol2string (LAtom val) = return $ LString val
symbol2string x           = throwError $ TypeMismatch "symbol" x

car [LList (x:xs)] = return x
car [LDottedList (x:xs) _] = return x
-- Next case also handles (car '())
car [badArg] = throwError $ TypeMismatch "pair" badArg -- TODO: Why not "list"?
car badArgs = throwError $ NumArgs 1 badArgs

cdr [LList (x:xs)] = return $ LList xs
cdr [LDottedList [x]    y] = return y
cdr [LDottedList (_:xs) y] = return $ LDottedList xs y
-- Next case also handles (cdr '())
cdr [badArg] = throwError $ TypeMismatch "pair" badArg -- TODO: Why not "list"?
cdr badArgs = throwError $ NumArgs 1 badArgs

cons [x, LList []] = return $ LList [x]
cons [x, LList ys] = return $ LList (x:ys)
cons [x, LDottedList ys lasty] = return $ LDottedList (x:ys) lasty
cons [x, y] = return $ LDottedList [x] y
cons badArgs = throwError $ NumArgs 2 badArgs

eqvp [x, y] = return $ LBool $ eqvPair x y
eqvp badArgs = throwError $ NumArgs 2 badArgs

eqvPair (LBool x)   (LBool y)   = x == y
eqvPair (LNumber x) (LNumber y) = x == y
eqvPair (LString x) (LString y) = x == y
eqvPair (LAtom x)   (LAtom y)   = x == y
eqvPair (LList xs)  (LList ys)  = (length xs == length ys) &&
                                  (and $ zipWith eqvPair xs ys)
eqvPair (LDottedList xs lastx) (LDottedList ys lasty) = eqvPair 
                                                       (LList $ xs ++ [lastx])
                                                       (LList $ ys ++ [lasty])
eqvPair _ _ = False
