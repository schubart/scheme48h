module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import Monad
import System.Environment
import Numeric
import Data.Char
import Control.Monad.Error

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

-- Decided to prefix ctors with "L" to make it less confusing...
data LispVal = LAtom String
             | LList [LispVal]
             | LDottedList [LispVal] LispVal
             | LNumber Integer
             | LString String
             | LBool Bool
instance Show LispVal where show = showVal

parseAtom = do first <- symbol <|> letter
               rest  <- many $ letter <|> symbol <|> digit
               return $ LAtom $ first : rest

parseQuoted = do char '\''
                 x <- parseExpr
                 return $ LList [LAtom "quote", x] 

parseList = do items <- sepBy parseExpr spaces
               return $ LList items

parseDottedList = do init <- endBy parseExpr spaces
                     last <- char '.' >> spaces >> parseExpr
                     return $ LDottedList init last

parseNumber''' = liftM (LNumber . read) $ many1 digit

-- Exercise 1.1: parseNumber with do notation.
parseNumber'' = do digits <- many1 digit
                   return $ LNumber $ read digits

-- Exercise 1.2: parseNumber with >>= notation.
parseNumber' = (many1 digit) >>= (return . LNumber . read)

-- Note: Exercise 1.4: I didn't like that the solution silently
-- changes the definition of a symbol...

parseNumber = parseDec1 
              <|> parseDec2 
              <|> parseBin
              <|> parseOct
              <|> parseHex

parseDec1 = do digits <- many1 digit
               return $ LNumber $ read digits

parseDec2 = do try $ string "#d"
               digits <- many1 digit
               return $ LNumber $ read digits

parseBin = do try $ string "#b"
              digits <- many1 $ oneOf "01"
              return $ LNumber $ readBase 2 digits

parseOct = do try $ string "#o"
              digits <- many1 $ oneOf "01234567"
              return $ LNumber $ readBase 8 digits

parseHex = do try $ string "#x"
              digits <- many1 $ oneOf "01234567890abcdef"
              return $ LNumber $ readBase 16 digits

readBase base digits = fst $ head $ readInt base (const True) digitToInt digits

-- TODO: Exercises 1.5, 1.6, 1.7

-- Exercises 1.2, .13: Escaped strings.
parseUnescapedChar :: Parser Char
parseUnescapedChar = do  do { try (string "\\\"");  return '"' }
                     <|> do { try (string "\\n");   return '\n' }
                     <|> do { try (string "\\t");   return '\t' }
                     <|> do { try (string "\\r");   return '\r' }
                     <|> do { try (string "\\\\");  return '\\' }
                     <|> noneOf "\""

parseString = do char '"'
                 x <- many $ parseUnescapedChar
                 char '"'
                 return $ LString x

parseBool = do char '#'
               val <- oneOf "tf"
               return $ case val of
                          't' -> LBool True
                          'f' -> LBool False

parseExpr = parseAtom
            <|> parseQuoted
            <|> do char '('
                   x <- try parseList <|> parseDottedList
                   char ')'
                   return x
            <|> parseNumber
            <|> parseString
            <|> parseBool

-- TODO: Exercises 2.*

showVal :: LispVal -> String
showVal (LAtom name)            = name
showVal (LList items)           = "(" ++ unwordsList items ++ ")"
showVal (LDottedList init last) = "(" 
                                  ++ unwordsList init 
                                  ++ " . " 
                                  ++ showVal last 
                                  ++ ")"
showVal (LNumber value)         = show value
showVal (LString value)         = "\"" ++ value ++ "\""
showVal (LBool True)            = "#t"
showVal (LBool False)           = "#f"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- Evaluation

eval :: LispVal -> ThrowsError LispVal
eval val@(LString _)                  = return val
eval val@(LNumber _)                  = return val
eval val@(LBool _)                    = return val
eval (LList [LAtom "quote", val])     = return val
eval (LList [LAtom "if", cond, t, e]) = do result <- eval cond
                                           case result of
                                             LBool True  -> eval t
                                             LBool False -> eval e
                                             x -> throwError $ 
                                                  TypeMismatch "boolean" x
eval (LList (LAtom "if" : x))         = throwError $ NumArgs 3 x
eval (LList (LAtom func : args))      = mapM eval args >>= apply func
eval badForm                          = throwError $ UnrecognizedSpecialForm badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ UnknownFunction func) 
                        ($ args)
                        (lookup func primitives)

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

-- Exception handling.

-- Error types.
data LispError = Parser ParseError
               | UnrecognizedSpecialForm LispVal
               | UnknownFunction String
               | NumArgs Int [LispVal]
               | TypeMismatch String LispVal
               | Default String

-- Make it an Haskell error.
-- TODO: What's noMsg and strMsg about?
instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default

type ThrowsError = Either LispError

-- Error messages.
instance Show LispError where
    show (Parser err) 
        = ("Parse error: " ++ show err)

    show (UnrecognizedSpecialForm form)
        = ("Unrecognized special form: " ++ show form)

    show (UnknownFunction name)
        = (name ++ ": Unknown function")

    show (NumArgs expected actual) 
        = ("Expected " ++ show expected ++ " arguments, " ++
           "got " ++ (show $ length actual) ++ 
           ": " ++ unwordsList actual)

    show (TypeMismatch expected actual)
        = ("Expected " ++ expected ++ ", " ++
           "got " ++ show actual)
    -- TODO: Why no definition for Default?

main :: IO ()
main = do args <- getArgs
          let evaluated = (readExpr $ head args) >>= eval
          case evaluated of
            Left err  -> print err
            Right val -> print val

readExpr input = case parse parseExpr "lisp" input of
                   Left  err -> throwError $ Parser err
                   Right val -> return val
