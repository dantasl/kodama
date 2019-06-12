module Interpreter where

{- Módulo responsável por interpretar 
o resultado do parser -}

import Lexer
import Tokens

data Expression = MathExpression Token
                | BooleanExpression Token
                | StringExpression Token
                deriving (Show)

data Statement = Assignment Token Expression
          | VarDeclaration Token Expression
          | Print Expression
          | Println Expression
          | Read Token
          | If Expression Statement Statement
          | Chain [Statement]
          deriving (Show)

extractToken :: Expression -> Token
extractToken (MathExpression x) = x
extractToken (BooleanExpression x) = x
extractToken (StringExpression x) = x