module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import Monad
import System.Environment
import Numeric
import Data.Char

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

-- Decided to prefix ctors with "L" to make it less confusing...
data LispVal = LAtom String
             | LList [LispVal]
             | LDottetList [LispVal] LispVal
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
                     return $ LDottetList init last

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

-- "lisp" only seems to be used for error messages.
readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left  err -> LString $ "No match: " ++ show err
                   Right val -> val

showVal :: LispVal -> String
showVal (LAtom name)            = name
showVal (LList items)           = "(" ++ unwordsList items ++ ")"
showVal (LDottetList init last) = "(" 
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

eval :: LispVal -> LispVal
eval val@(LString _)              = val
eval val@(LNumber _)              = val
eval val@(LBool _)                = val
eval (LList [LAtom "quote", val]) = val
eval (LList (LAtom func : args))  = apply func $ map eval args

err object message = error $ (show object) ++ ": " ++ message

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (err func "Not defined") ($ args) $ lookup func primitives

primitives ::[(String, [LispVal] -> LispVal)]
primitives = [("+",         numericBinop (+)),
              ("-",         numericBinop (-)),
              ("*",         numericBinop (*)),
              ("/",         numericBinop div),
              ("mod",       numericBinop mod),
              ("quotient",  numericBinop quot),
              ("remainder", numericBinop rem),
              -- Exercise 3.1
              ("boolean?", booleanp),
              ("symbol?",  symbolp),
              ("string?",  stringp),
              ("number?",  numberp),
              -- Exercise 3.3
              ("string->symbol", string2symbol),
              ("symbol->string", symbol2string)
             ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op args = LNumber $ foldl1 op $ map unpackNum args

unpackNum :: LispVal -> Integer
unpackNum (LNumber n) = n
{- Exercise 3.2
unpackNum (LString n) = let parsed = reads n in
                        if null parsed
                        then 0
                        else fst $ head $ parsed
unpackNum (LList [n]) = unpackNum n
-}
unpackNum x           = err x "Not a number"

-- Exercise solutions throw exception (non-exhaustive pattern) when calling
-- these unary functions with multiple args. This here returns false.
booleanp [(LBool _)] = LBool True
booleanp _           = LBool False
symbolp [(LAtom _)] = LBool True
symbolp _           = LBool False
stringp [(LString _)] = LBool True
stringp _             = LBool False
numberp [(LNumber _)] = LBool True
numberp _             = LBool False
string2symbol [(LString val)] = LAtom val
string2symbol x = err x "Not a string"
symbol2string [(LAtom val)] = LString val
symbol2string x = err x "Not a symbol"


main :: IO ()
main = do getArgs >>= putStrLn . show . eval . readExpr . head
