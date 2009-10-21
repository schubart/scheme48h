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
               deriving Show

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
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left  err -> "No match: " ++ show err
                   Right val -> show val

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ readExpr $ head args
