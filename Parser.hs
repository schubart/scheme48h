module Parser (readExpr) where

import Types
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric (readInt)
import Data.Char (digitToInt)
import Control.Monad.Error

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

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

readExpr input = case parse parseExpr "lisp" input of
                   Left  err -> throwError $ Parser err
                   Right val -> return val
