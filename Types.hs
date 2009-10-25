module Types
    ( Env
    , LispVal(..)
    , LispError(..)
    , ThrowsError
    , IOThrowsError)
where

import IO (Handle)
import Data.IORef (IORef)
import Text.ParserCombinators.Parsec (ParseError)
import Control.Monad.Error (Error(..), ErrorT)

type Env = IORef [(String, IORef LispVal)]

data LispVal = LAtom String
             | LList [LispVal]
             | LDottedList [LispVal] LispVal
             | LNumber Integer
             | LString String
             | LBool Bool
             | PrimitiveFunc ([LispVal] ->   ThrowsError LispVal)
             | IOFunc        ([LispVal] -> IOThrowsError LispVal)
             | Func { params  :: [String]
                    , vararg  :: (Maybe String)
                    , body    :: [LispVal]
                    , closure :: Env }
             | Port Handle

instance Show LispVal where
    show (LAtom name)              = name
    show (LList items)             = "(" ++ unwordsList items ++ ")"
    show (LDottedList init last)   = "(" 
                                     ++ unwordsList init 
                                     ++ " . " 
                                     ++ show last 
                                     ++ ")"
    show (LNumber value)           = show value
    show (LString value)           = "\"" ++ value ++ "\""
    show (LBool True)              = "#t"
    show (LBool False)             = "#f"
    show (PrimitiveFunc _)         = "<primitive>"
    show (Func { params = params
               , vararg = vararg}) = ("(lambda (" 
                                      ++ unwords params 
                                      ++ case vararg of
                                           Nothing -> ""
                                           Just arg -> " ." ++ arg
                                      ++ ") ...)")
    show (IOFunc _)                = "<IO primitive>"
    show (Port _)                  = "<IO handle>"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

data LispError = Parser ParseError
               | UnrecognizedSpecialForm LispVal
               | UnknownFunction String
               | NumArgs Int [LispVal]
               | TypeMismatch String LispVal
               | UnboundVar String
               | Default String

instance Show LispError where
    show (Parser err) = "Parse error: " ++ show err

    show (UnrecognizedSpecialForm form) = "Unrecognized special form: " 
                                          ++ show form

    show (UnknownFunction name) = name ++ ": Unknown function"

    show (NumArgs expected actual) = "Expected " ++ show expected 
                                     ++ " arguments, got " 
                                     ++ (show $ length actual) ++ 
                                     ": " ++ unwordsList actual

    show (TypeMismatch expected actual) = "Expected " ++ expected 
                                          ++ ", got " ++ show actual

    show (UnboundVar var) = var ++ ": Unbound variable"
    -- TODO: Why no definition for Default?

-- TODO: What's noMsg and strMsg about?
instance Error LispError where
    noMsg = Default "An error has occurred"
    strMsg = Default

type ThrowsError = Either LispError

type IOThrowsError = ErrorT LispError IO
