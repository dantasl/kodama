module Interpreter where

{- Módulo responsável por interpretar 
o resultado do parser -}

import qualified Lexer as Lexer
import Values
import Memory

import Control.Monad
import Control.Monad.State

data Expression = MathExpression MExpression
                | BooleanExpression BExpression
                | StringExpression SExpression
                deriving (Show)

data MathOperator = Addition 
                  | Multiplication
                  | Division
                  | Subtraction
                  | Power
                  | Mod
                  | Negate
                  deriving (Show)

data BooleanOperator = And
                     | Or
                     | Not
                     deriving (Show)

data MExpression = MValue Lexer.Token
                 | MId Lexer.Token
                 | UnaryMExpression MathOperator MExpression
                 | BinaryMExpression MathOperator MExpression MExpression
                 deriving (Show)

data BExpression = BValue Lexer.Token
                 | BId Lexer.Token
                 | UnaryBExpression BooleanOperator BExpression
                 | BinaryBExpression BooleanOperator BExpression BExpression
                 deriving (Show)

data SExpression = SValue Lexer.Token
                    deriving (Show)

data Statement = Assignment Lexer.Token Expression
               | VarDeclaration Lexer.Token Expression
               | Print Expression
               | Println Expression
               | Read Lexer.Token
               | If Expression Statement Statement
               | While Expression Statement
               | Chain [Statement]
               deriving (Show)

getBooleanOpFromToken :: Lexer.Token -> BooleanOperator
getBooleanOpFromToken (Lexer.And _) = And
getBooleanOpFromToken (Lexer.Or _) = Or
getBooleanOpFromToken (Lexer.Not _) = Not

getMathOpFromToken :: Lexer.Token -> MathOperator
getMathOpFromToken (Lexer.Plus _) = Addition
getMathOpFromToken (Lexer.Minus _) = Subtraction
getMathOpFromToken (Lexer.Multiply _) = Multiplication
getMathOpFromToken (Lexer.Divide _) = Division
getMathOpFromToken (Lexer.Power _) = Power
getMathOpFromToken (Lexer.Mod _) = Mod

extractToken :: Expression -> Lexer.Token
extractToken (MathExpression x) = (Lexer.ID "" (1,1))
extractToken (BooleanExpression x) = (Lexer.ID "" (1,1))
extractToken (StringExpression x) = (Lexer.ID "" (1,1))

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

evalStmts while@(While b body) = do
    scope <- get
    when (extractBooleanValue $ extractValue $ extractToken b) $ evalStmts body >> evalStmts while