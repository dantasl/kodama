module Interpreter where

{- Módulo responsável por interpretar 
o resultado do parser -}

import qualified Lexer as Lexer
import Values
import Tokens
import Memory

import Control.Monad
import Control.Monad.State

data Expression = MathExpression Lexer.Token
                | BooleanExpression Lexer.Token
                | StringExpression Lexer.Token
                deriving (Show)

data Statement = Assignment Lexer.Token Expression
          | VarDeclaration Lexer.Token Expression
          | Print Expression
          | Println Expression
          | Read Lexer.Token
          | If Expression Statement Statement
          | Chain [Statement]
          deriving (Show)

extractToken :: Expression -> Lexer.Token
extractToken (MathExpression x) = x
extractToken (BooleanExpression x) = x
extractToken (StringExpression x) = x

extractBooleanValue :: Value -> Bool
extractBooleanValue (Bool x) = x

evaluate :: [Statement] -> StateT Memory IO ()
evaluate stmts = mapM_ evalStmts stmts

evalStmts :: Statement -> StateT Memory IO ()

evalStmts (Chain stmts) = mapM_ evalStmts stmts

evalStmts (VarDeclaration t e) =
    modify (symtableInsert (t, extractToken e))

evalStmts (Assignment t e) =
    modify (symtableUpdate (t, extractToken e))

evalStmts (Print e) = do
    lift $ putStr $ show $ extractValue $ extractToken e

evalStmts (Println e) = do
    lift $ print $ extractValue $ extractToken e

evalStmts (If b s1 s2) = do
    if extractBooleanValue $ extractValue $ extractToken b
        then evalStmts s1
        else evalStmts s2